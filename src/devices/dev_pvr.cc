/*
 *  Copyright (C) 2006-2014  Anders Gavare.  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright  
 *     notice, this list of conditions and the following disclaimer in the 
 *     documentation and/or other materials provided with the distribution.
 *  3. The name of the author may not be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE   
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 *  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
 *   
 *
 *  COMMENT: PowerVR CLX2 (graphics controller used in the Dreamcast)
 *
 *  Implemented by reading http://www.ludd.luth.se/~jlo/dc/powervr-reg.txt and
 *  http://mc.pp.se/dc/pvr.html, source code of various demos and KalistOS,
 *  attempting to run the PROM from my own Dreamcast, and doing a lot of
 *  guessing.
 *
 *  TODO: Almost everything
 *
 *	x)  Change resolution during runtime (PAL/NTSC/???)
 *
 *	x)  Lots of work on the 3D "Tile Accelerator" engine.
 *		Recognize commands and turn into OpenGL or similar
 *		commands on the host?
 *		Wire-frame when running on a host without XGL?
 *
 *  Multiple lists of various kinds (6?).
 *  Lists growing downwards!
 *  Pixel clip for rendering.
 *  Real Rendering, using OpenGL if possible.
 *  Tile bins... with 6 pointers for each tile (?)
 *  PVR DMA.
 *  Textures.
 *  ...
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cpu.h"
#include "device.h"
#include "devices.h"
#include "float_emul.h"
#include "machine.h"
#include "memory.h"
#include "misc.h"
#include "timer.h"

#include "thirdparty/dreamcast_pvr.h"
#include "thirdparty/dreamcast_sysasicvar.h"


/* For debugging: */
//#define DEBUG_RENDER_AS_WIRE_FRAME	// Renders 3-corner textured polygons as wire-frame
//#define	TA_DEBUG		// Dumps TA commands
//#define debug fatal			// Dumps debug even without -v.

#define	INTERNAL_FB_ADDR	0x300000000ULL
#define	PVR_FB_TICK_SHIFT	18

#define	PVR_VBLANK_HZ		60.0

#define	PVR_MARGIN		16

#define	VRAM_SIZE		(8*1048576)

/*  DMA:  */
#define	PVR_DMA_MEMLENGTH	0x100
#define	N_PVR_DMA_REGS		(PVR_DMA_MEMLENGTH / sizeof(uint32_t))

#define	PVR_ADDR		0x00
#define	PVR_COUNT		0x04
#define	PVR_MODE		0x08
#define	PVR_LMMODE0		0x84  
#define	PVR_LMMODE1		0x88


struct pvr_data {
	struct vfb_data		*fb;
	int			fb_update_x1;
	int			fb_update_y1;
	int			fb_update_x2;
	int			fb_update_y2;

	struct timer		*vblank_timer;
	int			vblank_interrupts_pending;

	/*  PVR registers:  */
	uint32_t		reg[PVRREG_REGSIZE / sizeof(uint32_t)];

	/*  Calculated by pvr_geometry_updated():  */
	int			xsize, ysize;
	int			bytes_per_pixel;

	/*  Cached values (from registers):  */
	/*  DIWMODE:  */
	int			clock_double;
	int			strip_buffer_enabled;
	int			strip_length;
	int			argb8888_threshold;
	int			extend;
	int			pixelmode;
	int			line_double;
	int			display_enabled;
	/*  BRDCOLR:  */
	int			border_updated;
	/*  SYNCCONF:  */
	int			video_enabled;
	int			broadcast_standard;
	int			interlaced;
	int			h_sync_positive;
	int			v_sync_positive;
	/*  TILEBUF_SIZE:  */
	int			tilebuf_xsize;
	int			tilebuf_ysize;

	/*  Current Tile Accelerator Command (64 bytes):  */
	uint32_t		ta[64 / sizeof(uint32_t)];

	/*  Stored list of TA commands:  */
	int			current_list_type;
	uint32_t		*ta_commands;
	size_t			allocated_ta_commands;
	size_t			n_ta_commands;

	/*  Video RAM and Z information:  */
	uint8_t			*vram;
	double			*vram_z;

	/*  DMA registers:  */
	uint32_t		dma_reg[N_PVR_DMA_REGS];
	uint32_t		dma_more_reg[N_PVR_DMA_REGS];
};

struct pvr_data_alt {
	struct pvr_data		*d;
};


#define	REG(x)		(d->reg[(x)/sizeof(uint32_t)])
#define	DEFAULT_WRITE	REG(relative_addr) = idata;


/*  Forward declaration.  */
DEVICE_ACCESS(pvr_ta);


void pvr_dma_transfer(struct cpu *cpu, struct pvr_data *d)
{
	const int channel = 2;
	uint32_t sar = cpu->cd.sh.dmac_sar[channel] & 0x1fffffff;
	uint32_t dar = cpu->cd.sh.dmac_dar[channel] & 0x1fffffff;
	uint32_t count = cpu->cd.sh.dmac_tcr[channel] & 0x1fffffff;
	uint32_t chcr = cpu->cd.sh.dmac_chcr[channel];
	int transmit_size = 1;
	int src_delta = 0, dst_delta = 0;
	int cause_interrupt = chcr & CHCR_IE;

#if 0
	// Dump all SH4 DMA channels, for debugging:
	for (int dmaChannel = 0; dmaChannel < 4; ++dmaChannel)
	{
		fatal("{# dma channel %i: sar=%08x dar=%08x count=%08x chcr=%08x #}\n",
		    dmaChannel,
		    cpu->cd.sh.dmac_sar[dmaChannel],
		    cpu->cd.sh.dmac_dar[dmaChannel],
		    cpu->cd.sh.dmac_tcr[dmaChannel],
		    cpu->cd.sh.dmac_chcr[dmaChannel]);
	}
#endif

	/*  DMAC not enabled?  */
	if (!(chcr & CHCR_TD)) {
		fatal("pvr_dma_transfer: SH4 dma not enabled?\n");
		exit(1);
	}

	/*  Transfer End already set? Then don't transfer again.  */
	if (chcr & CHCR_TE)
		return;

	/*  Special case: 0 means 16777216:  */
	if (count == 0)
		count = 16777216;

	switch (chcr & CHCR_TS) {
	case CHCR_TS_8BYTE: transmit_size = 8; break;
	case CHCR_TS_1BYTE: transmit_size = 1; break;
	case CHCR_TS_2BYTE: transmit_size = 2; break;
	case CHCR_TS_4BYTE: transmit_size = 4; break;
	case CHCR_TS_32BYTE: transmit_size = 32; break;
	default: fatal("Unimplemented transmit size?! CHCR[%i] = 0x%08x\n",
	    channel, chcr);
	exit(1);
	}

	switch (chcr & CHCR_DM) {
	case CHCR_DM_FIXED:       dst_delta = 0; break;
	case CHCR_DM_INCREMENTED: dst_delta = 1; break;
	case CHCR_DM_DECREMENTED: dst_delta = -1; break;
	default: fatal("Unimplemented destination delta?! CHCR[%i] = 0x%08x\n",
	    channel, chcr);
	exit(1);
	}

	switch (chcr & CHCR_SM) {
	case CHCR_SM_FIXED:       src_delta = 0; break;
	case CHCR_SM_INCREMENTED: src_delta = 1; break;
	case CHCR_SM_DECREMENTED: src_delta = -1; break;
	default: fatal("Unimplemented source delta?! CHCR[%i] = 0x%08x\n",
	    channel, chcr);
	exit(1);
	}

	src_delta *= transmit_size;
	dst_delta *= transmit_size;

	switch (chcr & CHCR_RS) {
	case 0x200:
		dar = d->dma_reg[PVR_ADDR / sizeof(uint32_t)];

		if (dar != 0x10000000) {
			//fatal("[ NOTE: DMA to non-TA: dar=%08x (delta %i), sar=%08x (delta %i) ]\n",
			//    (int)dar, (int)dst_delta, (int)sar, (int)src_delta);
			dar = 0x04000000 | (dar & 0x007fffff);
			if (dst_delta == 0)
				dst_delta = src_delta;

			uint8_t *buf = (uint8_t*) malloc(transmit_size);
			while (count > 0) {
				// printf("sar = %08x dar = %08x\n", (int)sar, (int)dar);
				
				cpu->memory_rw(cpu, cpu->mem, sar, buf,
				    transmit_size, MEM_READ, NO_EXCEPTIONS | PHYSICAL);
				// for (int i = 0; i < transmit_size; ++i)
				// 	printf("%02x ", buf[i]);
				// printf("\n");

				cpu->memory_rw(cpu, cpu->mem, dar, buf,
				    transmit_size, MEM_WRITE, NO_EXCEPTIONS | PHYSICAL);

				count --;
				sar += src_delta;
				dar += dst_delta;
			}

			free(buf);

			break;
		} else {
			while (count > 0) {
				unsigned char buf[sizeof(uint32_t)];
				int ofs;
				size_t chunksize = transmit_size;

				if (chunksize > sizeof(uint32_t))
					chunksize = sizeof(uint32_t);

				for (ofs = 0; ofs < transmit_size; ofs += chunksize) {
					cpu->memory_rw(cpu, cpu->mem, sar + ofs, buf,
					    chunksize, MEM_READ, NO_EXCEPTIONS | PHYSICAL);

					dev_pvr_ta_access(cpu, cpu->mem, ofs, buf, chunksize,
					    MEM_WRITE, d);
				}

				count --;
				sar += src_delta;
			}
		}

		// Transfer End. TODO: _EXACTLY_ what happens at the end of
		// a transfer?
		cpu->cd.sh.dmac_chcr[channel] |= CHCR_TE;
		cpu->cd.sh.dmac_chcr[channel] &= ~CHCR_TD;
		cpu->cd.sh.dmac_sar[channel] = sar;
		cpu->cd.sh.dmac_tcr[channel] = count;

		// d->dma_reg[PVR_ADDR / sizeof(uint32_t)] = ???;
		d->dma_reg[PVR_COUNT / sizeof(uint32_t)] = 0;

		SYSASIC_TRIGGER_EVENT(SYSASIC_EVENT_PVR_DMA);

		break;
	default:
		fatal("Unimplemented SH4 RS DMAC: 0x%08x (PVR)\n",
		    (int) (chcr & CHCR_RS));
		exit(1);
	}

	if (cause_interrupt) {
		fatal("TODO: pvr sh4 dmac interrupt!\n");
		exit(1);
	}
}


DEVICE_ACCESS(pvr_dma)
{
	struct pvr_data *d = (struct pvr_data *) extra;
	uint64_t idata = 0, odata = 0;

	if (writeflag == MEM_WRITE)
		idata = memory_readmax64(cpu, data, len);

	/*  Default read:  */
	if (writeflag == MEM_READ)
		odata = d->dma_reg[relative_addr / sizeof(uint32_t)];

	switch (relative_addr) {

	case PVR_ADDR:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr_dma: ADDR set to 0x%08x ]\n",
			    (int) idata);
		}
		break;

	case PVR_COUNT:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr_dma: COUNT set to 0x%08x ]\n",
			    (int) idata);
		}
		break;

	case PVR_MODE:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr_dma: MODE set to 0x%08x ]\n",
			    (int) idata);
			if (idata != 0) {
				pvr_dma_transfer(cpu, d);
				idata = 0;
			}
		}
		break;

	/*  These are written to by the Dreamcast ROM, but I have not
	    found them documented anywhere.  */
	case 0x10:
	case 0x14:
		if (writeflag == MEM_WRITE && idata != 0x0cff0000) {
			fatal("[ pvr_dma: TODO: unknown_0x%02x set to "
			    "0x%08x ]\n", (int) relative_addr, (int) idata);
			exit(1);
		}
		break;

	case 0x18:
	case 0x1c:
	case 0x20:
	case 0x40:
	case 0x44:
	case 0x48:
	case 0x4c:
		if (writeflag == MEM_WRITE && idata != 0) {
			fatal("[ pvr_dma: TODO: unknown_0x%02x set to "
			    "0x%08x ]\n", (int) relative_addr, (int) idata);
			exit(1);
		}
		break;

	case PVR_LMMODE0:	/*  0x84  */
		if (writeflag == MEM_WRITE && idata != 0) {
			fatal("[ pvr_dma: TODO: LMMODE0 set to "
			    "0x%08x ]\n", (int) idata);
			exit(1);
		}
		break;

	case PVR_LMMODE1:	/*  0x88  */
		if (writeflag == MEM_WRITE && idata != 0) {
			fatal("[ pvr_dma: TODO: LMMODE1 set to "
			    "0x%08x ]\n", (int) idata);
			exit(1);
		}
		break;

	case 0x8c:
		if (writeflag == MEM_WRITE) {
			fatal("[ pvr_dma: write to 0x8c: TODO ]\n");
			exit(1);
		} else {
			/*  0x20 means G2 DMA in progress?  */
			/*  0x11 = mask which has to do with AICA  */
			odata = 0x11 * (random() & 1);
		}
		break;

	case 0x9c:
		if (writeflag == MEM_WRITE && idata != 0) {
			fatal("[ pvr_dma: TODO: unknown_0x%02x set to "
			    "0x%08x ]\n", (int) relative_addr, (int) idata);
			exit(1);
		}
		break;

	case 0xa0:
		if (writeflag == MEM_WRITE && idata != 0x80000000) {
			fatal("[ pvr_dma: TODO: unknown_0x%02x set to "
			    "0x%08x ]\n", (int) relative_addr, (int) idata);
			exit(1);
		}
		break;

	case 0xa4:
	case 0xac:
		if (writeflag == MEM_WRITE && idata != 0) {
			fatal("[ pvr_dma: TODO: unknown_0x%02x set to "
			    "0x%08x ]\n", (int) relative_addr, (int) idata);
			exit(1);
		}
		break;

	default:if (writeflag == MEM_READ) {
			fatal("[ pvr_dma: read from addr 0x%x ]\n",
			    (int)relative_addr);
		} else {
			fatal("[ pvr_dma: write to addr 0x%x: 0x%x ]\n",
			    (int)relative_addr, (int)idata);
		}

		exit(1);
	}

	/*  Default write:  */
	if (writeflag == MEM_WRITE)
		d->dma_reg[relative_addr / sizeof(uint32_t)] = idata;

	if (writeflag == MEM_READ)
		memory_writemax64(cpu, data, len, odata);

	return 1;
}


DEVICE_ACCESS(pvr_dma_more)
{
	struct pvr_data *d = (struct pvr_data *) extra;
	uint64_t idata = 0, odata = 0;

	if (writeflag == MEM_WRITE)
		idata = memory_readmax64(cpu, data, len);

	/*  Default read:  */
	if (writeflag == MEM_READ)
		odata = d->dma_more_reg[relative_addr / sizeof(uint32_t)];

	switch (relative_addr) {

	case 0x00:	// 0x04ff0000
	case 0x04:	// 0x0cff0000
	case 0x08:	// 0x00000020
	case 0x0c:	// 0x00000000
	case 0x10:	// 0x00000000
	case 0x80:	// 0x67027f00
		break;

	case 0x14:
	case 0x18:
		if (writeflag == MEM_WRITE && idata != 0)
		{
			fatal("PVR other DMA mode (?):\n");
			fatal("0x00: %08x\n", d->dma_more_reg[0x00/4]);
			fatal("0x04: %08x\n", d->dma_more_reg[0x04/4]);
			fatal("0x08: %08x\n", d->dma_more_reg[0x08/4]);
			fatal("0x0c: %08x\n", d->dma_more_reg[0x0c/4]);
			fatal("0x10: %08x\n", d->dma_more_reg[0x10/4]);
			fatal("0x14: %08x\n", d->dma_more_reg[0x14/4]);
			exit(1);
		}
		break;

	default:if (writeflag == MEM_READ) {
			fatal("[ pvr_dma_more: read from addr 0x%x ]\n",
			    (int)relative_addr);
		} else {
			fatal("[ pvr_dma_more: write to addr 0x%x: 0x%x ]\n",
			    (int)relative_addr, (int)idata);
		}

		exit(1);
	}

	/*  Default write:  */
	if (writeflag == MEM_WRITE)
		d->dma_more_reg[relative_addr / sizeof(uint32_t)] = idata;

	if (writeflag == MEM_READ)
		memory_writemax64(cpu, data, len, odata);

	return 1;
}


/*
 *  pvr_fb_invalidate():
 */
void pvr_fb_invalidate(struct pvr_data *d, int start, int stop)
{
	d->fb_update_x1 = d->fb_update_y1 = 0;
	d->fb_update_x2 = d->xsize - 1;
	d->fb_update_y2 = d->ysize - 1;
}


/*
 *  pvr_vblank_timer_tick():
 *
 *  This function is called PVR_VBLANK_HZ times per real-world second. Its job
 *  is to fake vertical retrace interrupts.
 */     
static void pvr_vblank_timer_tick(struct timer *t, void *extra)
{
	struct pvr_data *d = (struct pvr_data *) extra;
	d->vblank_interrupts_pending ++;
}


/*
 *  pvr_geometry_updated():
 *
 *  This function should be called every time a register is written to which
 *  affects the framebuffer geometry (size, bit-depth, starting position, etc).
 */
void pvr_geometry_updated(struct pvr_data *d)
{
	int old_xsize = d->xsize, old_ysize = d->ysize, oldbpp = d->bytes_per_pixel;

	/*  Make sure to redraw border on geometry changes.  */
	d->border_updated = 1;

	d->xsize = (REG(PVRREG_DIWSIZE) >> DIWSIZE_DPL_SHIFT) & DIWSIZE_MASK;
	d->ysize = (REG(PVRREG_DIWSIZE) >> DIWSIZE_LPF_SHIFT) & DIWSIZE_MASK;

	/*  E.g. 319x479  =>  320x480  */
	d->xsize = (d->xsize + 1) * sizeof(uint32_t);
	d->ysize ++;

	switch (d->pixelmode) {
	case 0:
	case 1:	d->bytes_per_pixel = 2; break;
	case 2:	d->bytes_per_pixel = 3; break;
	case 3:	d->bytes_per_pixel = 4; break;
	}

	d->xsize /= d->bytes_per_pixel;

	if (REG(PVRREG_DIWCONF) & DIWCONF_LR)
		d->xsize /= 2;

	if (d->line_double)
		d->ysize /= 2;

	bool settingsChanged = d->xsize != old_xsize ||
	    d->ysize != old_ysize ||
	    d->bytes_per_pixel != oldbpp;

	if (!settingsChanged)
		return;

	/*  Scrap Z buffer if we have one.  */
	if (d->vram_z == NULL) {
		free(d->vram_z);
		d->vram_z = NULL;
	}
		
	/*  Only show geometry debug message if output is enabled:  */
	if (!d->video_enabled || !d->display_enabled)
		return;

	debug("[ pvr_geometry_updated: %i x %i, ", d->xsize, d->ysize);

	switch (d->pixelmode) {
	case 0: debug("RGB0555 (16-bit)"); break;
	case 1: debug("RGB565 (16-bit)"); break;
	case 2: debug("RGB888 (24-bit)"); break;
	case 3: debug("RGB0888 (32-bit)"); break;
	}

	debug(" ]\n");
}


#ifdef DEBUG_RENDER_AS_WIRE_FRAME
/*  Ugly quick-hack:  */
static void line(struct pvr_data *d, int x1, int y1, int x2, int y2)
{
	int fb_base = REG(PVRREG_FB_RENDER_ADDR1);
	int i;
	for (i=0; i<256; i++) {
		int px = (i * x2 + (256-i) * x1) >> 8;
		int py = (i * y2 + (256-i) * y1) >> 8;
		if (px > 0 && py > 0 && px < d->xsize && py < d->ysize) {
			int ofs = fb_base + (px + py * d->xsize) *
                            d->bytes_per_pixel;
			d->vram[(ofs+0) % VRAM_SIZE] = 255;
			d->vram[(ofs+1) % VRAM_SIZE] = 255;
		}
	}
}
#endif


// Ugly quick-hack z-buffer line drawer, for triangles.
// Assumes 16-bit color.
static void simpleline(struct pvr_data *d, int y, double x1, double x2,
	double z1, double z2, double r1, double r2, double g1, double g2,
	double b1, double b2)
{
	if (y < 0 || y >= d->ysize || (x1 < 0 && x2 < 0)
	     || (x1 >= d->xsize && x2 >= d->xsize))
		return;

	int fb_base = REG(PVRREG_FB_RENDER_ADDR1);
	if (x1 > x2) {
		double tmpf = x1; x1 = x2; x2 = tmpf;
		tmpf = z1; z1 = z2; z2 = tmpf;
		tmpf = r1; r1 = r2; r2 = tmpf;
		tmpf = g1; g1 = g2; g2 = tmpf;
		tmpf = b1; b1 = b2; b2 = tmpf;
	}
	
	double dz12 = (x2 - x1 != 0) ? ( (double)(z2 - z1) / (double)(x2 - x1) ) : 0;
	double dr12 = (x2 - x1 != 0) ? ( (double)(r2 - r1) / (double)(x2 - x1) ) : 0;
	double dg12 = (x2 - x1 != 0) ? ( (double)(g2 - g1) / (double)(x2 - x1) ) : 0;
	double db12 = (x2 - x1 != 0) ? ( (double)(b2 - b1) / (double)(x2 - x1) ) : 0;
	double z = z1, r = r1, g = g1, b = b1;
	for (int x = x1; x <= x2; ++x) {
		if (x > 0 && x < d->xsize) {
			int ofs = x + y * d->xsize;
			if (d->vram_z[ofs] <= z) {
				d->vram_z[ofs] = z;

				// NOTE/TODO: Hardcoded for 565 pixelformat.
				int ri = r, gi = g, bi = b;
				if (ri < 0) ri = 0; if (ri > 255) ri = 255;
				if (gi < 0) gi = 0; if (gi > 255) gi = 255;
				if (bi < 0) bi = 0; if (bi > 255) bi = 255;
				int color = ((ri >> 3) << 11) + ((gi >> 2) << 5) + (bi >> 3);

				int fbofs = fb_base + ofs * d->bytes_per_pixel;
				d->vram[(fbofs+0) % VRAM_SIZE] = color & 255;
				d->vram[(fbofs+1) % VRAM_SIZE] = color >> 8;
			}
		}
		
		z += dz12; r += dr12; g += dg12; b += db12;
	}
}

static void texturedline(struct pvr_data *d,
	int texture_pixelformat, bool twiddled, int stride,
	int texture, int texture_xsize, int texture_ysize,
	int y, int x1, int x2, double z1, double z2,
	double u1, double u2, double v1, double v2)
{
	if (y < 0 || y >= d->ysize || (x1 < 0 && x2 < 0)
	     || (x1 >= d->xsize && x2 >= d->xsize))
		return;

	int fb_base = REG(PVRREG_FB_RENDER_ADDR1);
	if (x1 > x2) {
		int tmp = x1; x1 = x2; x2 = tmp;
		double tmpf = z1; z1 = z2; z2 = tmpf;
		tmpf = u1; u1 = u2; u2 = tmpf;
		tmpf = v1; v1 = v2; v2 = tmpf;
	}

	int bytesperpixel = 2;

	switch (texture_pixelformat)
	{
	case 0:	// ARGB1555
	case 1:	// RGB565
	case 2: // ARGB4444
		bytesperpixel = 2;
		break;
	case 6:	// 8-bit palette
		bytesperpixel = 1;
		break;
	default:
		// TODO
		break;
	}

	int palette_cfg = d->reg[PVRREG_PALETTE_CFG / sizeof(uint32_t)] & PVR_PALETTE_CFG_MODE_MASK;

	double dz12 = (x2 - x1 != 0) ? ( (double)(z2 - z1) / (double)(x2 - x1) ) : 0;
	double du12 = (x2 - x1 != 0) ? ( (double)(u2 - u1) / (double)(x2 - x1) ) : 0;
	double dv12 = (x2 - x1 != 0) ? ( (double)(v2 - v1) / (double)(x2 - x1) ) : 0;

	double z = z1, u = u1, v = v1;

	for (int x = x1; x <= x2; ++x) {
		if (x > 0 && x < d->xsize) {
			int ofs = x + y * d->xsize;
			if (d->vram_z[ofs] <= z) {
				d->vram_z[ofs] = z;

				int fbofs = fb_base + ofs * d->bytes_per_pixel;

				// Get color from texture:
				int texturex = u * texture_xsize;
				texturex &= (texture_xsize-1);
				int texturey = v * texture_ysize;
				texturey &= (texture_ysize-1);

				int textureofs;
				if (twiddled) {
					texturex = 
					(texturex&1)|((texturex&2)<<1)|((texturex&4)<<2)|((texturex&8)<<3)|((texturex&16)<<4)|
					      ((texturex&32)<<5)|((texturex&64)<<6)|((texturex&128)<<7)|((texturex&256)<<8)|((texturex&512)<<9);
					texturey = 
					(texturey&1)|((texturey&2)<<1)|((texturey&4)<<2)|((texturey&8)<<3)|((texturey&16)<<4)|
					      ((texturey&32)<<5)|((texturey&64)<<6)|((texturey&128)<<7)|((texturey&256)<<8)|((texturey&512)<<9);
					textureofs = texturex * 2 + texturey;
				} else {
					if (stride > 0)
						textureofs = texturex + texturey * stride;
					else
						textureofs = texturex + texturey * texture_xsize;
				}

				textureofs *= bytesperpixel;	// 2 bytes per pixel.

				int addr = texture + textureofs;
				addr = ((addr & 4) << 20) | (addr & 3) | ((addr & 0x7ffff8) >> 1);

				int a = 255, r = 64, g = 64, b = 64;
				switch (texture_pixelformat)
				{
				case 0:	// ARGB1555:
					{
						int color = d->vram[addr] + (d->vram[addr+1] << 8);
						a = (color >> 15) & 0x1 ? 255 : 0;
						r = (color >> 10) & 0x1f;
						g = ((color >> 5) & 0x1f) << 1;
						b = (color) & 0x1f;
					}
					break;
				case 1:	// RGB565:
					{
						int color = d->vram[addr] + (d->vram[addr+1] << 8);
						r = (color >> 11) & 0x1f;
						g = (color >> 5) & 0x3f;
						b = (color) & 0x1f;
					}
					break;
				case 2:	// ARGB4444:
					{
						int color = d->vram[addr] + (d->vram[addr+1] << 8);
						a = ((color >> 12) & 15) * 0x11;
						r = ((color >> 8) & 15) << 1;
						g = ((color >> 4) & 15) << 2;
						b = ((color) & 15) << 1;
					}
					break;
				case 6:
					{
						// TODO: multiple palette banks?
						int index8bpp = d->vram[addr];
						char* base = (char*)&d->reg[PVRREG_PALETTE / sizeof(uint32_t)];
						uint16_t c16 = *((uint16_t*)base + index8bpp);
						uint16_t c32 = *((uint32_t*)base + index8bpp);
						switch (palette_cfg) {
						case PVR_PALETTE_CFG_MODE_ARGB1555:
							a = (c16 >> 15) & 0x1 ? 255 : 0;
							r = (c16 >> 10) & 0x1f;
							g = ((c16 >> 5) & 0x1f) << 1;
							b = (c16) & 0x1f;
							break;
						case PVR_PALETTE_CFG_MODE_RGB565:
							r = (c16 >> 11) & 0x1f;
							g = (c16 >> 5) & 0x3f;
							b = (c16) & 0x1f;
							break;
						case PVR_PALETTE_CFG_MODE_ARGB4444:
							a = ((c16 >> 12) & 15) * 0x11;
							r = ((c16 >> 8) & 15) << 1;
							g = ((c16 >> 4) & 15) << 2;
							b = ((c16) & 15) << 1;
							break;
						case PVR_PALETTE_CFG_MODE_ARGB8888:
							a = (c32 >> 24) & 255;
							r = ((c32 >> 16) & 255) >> 3;
							g = ((c32 >> 8) & 255) >> 2;
							b = ((c32) & 255) >> 3;
							break;
						}
					}
					break;
				default:
					fatal("pvr: unimplemented texture_pixelformat %i\n", texture_pixelformat);
					exit(1);
				}

				if (a == 255)
				{
					// Output as RGB565:
					// TODO: Support other formats.
					int color = (r << 11) + (g << 5) + (b);
					d->vram[(fbofs+0) % VRAM_SIZE] = color & 255;
					d->vram[(fbofs+1) % VRAM_SIZE] = color >> 8;
				} else if (a > 0) {
					int oldcolor = d->vram[(fbofs+0) % VRAM_SIZE];
					oldcolor += (d->vram[(fbofs+1) % VRAM_SIZE] << 8);
					int oldr = (oldcolor >> 11) & 0x1f;
					int oldg = (oldcolor >> 5) & 0x3f;
					int oldb = (oldcolor) & 0x1f;
					r = (a * r + oldr * (255 - a)) / 255;
					g = (a * g + oldg * (255 - a)) / 255;
					b = (a * b + oldb * (255 - a)) / 255;
					int color = (r << 11) + (g << 5) + (b);
					d->vram[(fbofs+0) % VRAM_SIZE] = color & 255;
					d->vram[(fbofs+1) % VRAM_SIZE] = color >> 8;
				}
			}
		}
		
		z += dz12;
		u += du12;
		v += dv12;
	}
}


// Slow software rendering, for debugging:
static void pvr_render_triangle(struct pvr_data *d,
	int x1, int y1, double z1, int r1, int g1, int b1,
	int x2, int y2, double z2, int r2, int g2, int b2,
	int x3, int y3, double z3, int r3, int g3, int b3)
{
	// Easiest if 1, 2, 3 are in order top to bottom.
	if (y2 < y1) {
		int tmp = x1; x1 = x2; x2 = tmp;
		tmp = y1; y1 = y2; y2 = tmp;
		tmp = r1; r1 = r2; r2 = tmp;
		tmp = g1; g1 = g2; g2 = tmp;
		tmp = b1; b1 = b2; b2 = tmp;
		double tmpf = z1; z1 = z2; z2 = tmpf;
	}

	if (y3 < y1) {
		int tmp = x1; x1 = x3; x3 = tmp;
		tmp = y1; y1 = y3; y3 = tmp;
		tmp = r1; r1 = r3; r3 = tmp;
		tmp = g1; g1 = g3; g3 = tmp;
		tmp = b1; b1 = b3; b3 = tmp;
		double tmpf = z1; z1 = z3; z3 = tmpf;
	}

	if (y3 < y2) {
		int tmp = x2; x2 = x3; x3 = tmp;
		tmp = y2; y2 = y3; y3 = tmp;
		tmp = r2; r2 = r3; r3 = tmp;
		tmp = g2; g2 = g3; g3 = tmp;
		tmp = b2; b2 = b3; b3 = tmp;
		double tmpf = z2; z2 = z3; z3 = tmpf;
	}

	if (y3 < 0 || y1 >= d->ysize)
		return;

	double dx12 = (y2-y1 != 0) ? ( (x2 - x1) / (double)(y2 - y1) ) : 0.0;
	double dx13 = (y3-y1 != 0) ? ( (x3 - x1) / (double)(y3 - y1) ) : 0.0;
	double dx23 = (y3-y2 != 0) ? ( (x3 - x2) / (double)(y3 - y2) ) : 0.0;

	double dz12 = (y2-y1 != 0) ? ( (z2 - z1) / (double)(y2 - y1) ) : 0.0;
	double dz13 = (y3-y1 != 0) ? ( (z3 - z1) / (double)(y3 - y1) ) : 0.0;
	double dz23 = (y3-y2 != 0) ? ( (z3 - z2) / (double)(y3 - y2) ) : 0.0;

	double dr12 = (y2-y1 != 0) ? ( (r2 - r1) / (double)(y2 - y1) ) : 0.0;
	double dr13 = (y3-y1 != 0) ? ( (r3 - r1) / (double)(y3 - y1) ) : 0.0;
	double dr23 = (y3-y2 != 0) ? ( (r3 - r2) / (double)(y3 - y2) ) : 0.0;

	double dg12 = (y2-y1 != 0) ? ( (g2 - g1) / (double)(y2 - y1) ) : 0.0;
	double dg13 = (y3-y1 != 0) ? ( (g3 - g1) / (double)(y3 - y1) ) : 0.0;
	double dg23 = (y3-y2 != 0) ? ( (g3 - g2) / (double)(y3 - y2) ) : 0.0;

	double db12 = (y2-y1 != 0) ? ( (b2 - b1) / (double)(y2 - y1) ) : 0.0;
	double db13 = (y3-y1 != 0) ? ( (b3 - b1) / (double)(y3 - y1) ) : 0.0;
	double db23 = (y3-y2 != 0) ? ( (b3 - b2) / (double)(y3 - y2) ) : 0.0;

	double startx = x1, startz = z1, startr = r1, startg = g1, startb = b1;
	double stopx = x1, stopz = z1, stopr = r1, stopg = g1, stopb = b1;
	for (int y = y1; y < y2; ++y)
	{
		simpleline(d, y, startx, stopx, startz, stopz, startr, stopr, startg, stopg, startb, stopb);
		startx += dx13; startz += dz13; startr += dr13; startg += dg13; startb += db13;
		stopx += dx12; stopz += dz12; stopr += dr12; stopg += dg12; stopb += db12;
	}

	stopx = x2; stopz = z2; stopr = r2; stopg = g2; stopb = b2;
	for (int y = y2; y < y3; ++y)
	{
		simpleline(d, y, startx, stopx, startz, stopz, startr, stopr, startg, stopg, startb, stopb);
		startx += dx13; startz += dz13; startr += dr13; startg += dg13; startb += db13;
		stopx += dx23; stopz += dz23; stopr += dr23; stopg += dg23; stopb += db23;
	}

#ifdef DEBUG_RENDER_AS_WIRE_FRAME
	// Wire-frame test:
	line(d, x1, y1, x2, y2);
	line(d, x1, y1, x3, y3);
	line(d, x2, y2, x3, y3);
#endif
}


// Slow software rendering, for debugging:
static void pvr_render_triangle_textured(struct pvr_data *d,
	int texture_pixelformat, bool twiddled, int stride,
	int texture, int texture_xsize, int texture_ysize,
	int x1, int y1, double z1, double u1, double v1,
	int x2, int y2, double z2, double u2, double v2,
	int x3, int y3, double z3, double u3, double v3)
{
	// Easiest if 1, 2, 3 are in order top to bottom.
	if (y2 < y1) {
		int tmp = x1; x1 = x2; x2 = tmp;
		tmp = y1; y1 = y2; y2 = tmp;
		double tmpf = z1; z1 = z2; z2 = tmpf;
		tmpf = u1; u1 = u2; u2 = tmpf;
		tmpf = v1; v1 = v2; v2 = tmpf;
	}

	if (y3 < y1) {
		int tmp = x1; x1 = x3; x3 = tmp;
		tmp = y1; y1 = y3; y3 = tmp;
		double tmpf = z1; z1 = z3; z3 = tmpf;
		tmpf = u1; u1 = u3; u3 = tmpf;
		tmpf = v1; v1 = v3; v3 = tmpf;
	}

	if (y3 < y2) {
		int tmp = x2; x2 = x3; x3 = tmp;
		tmp = y2; y2 = y3; y3 = tmp;
		double tmpf = z2; z2 = z3; z3 = tmpf;
		tmpf = u2; u2 = u3; u3 = tmpf;
		tmpf = v2; v2 = v3; v3 = tmpf;
	}

	if (y3 < 0 || y1 >= d->ysize)
		return;

	double dx12 = (y2-y1 != 0) ? ( (x2 - x1) / (double)(y2 - y1) ) : 0.0;
	double dx13 = (y3-y1 != 0) ? ( (x3 - x1) / (double)(y3 - y1) ) : 0.0;
	double dx23 = (y3-y2 != 0) ? ( (x3 - x2) / (double)(y3 - y2) ) : 0.0;

	double dz12 = (y2-y1 != 0) ? ( (z2 - z1) / (double)(y2 - y1) ) : 0.0;
	double dz13 = (y3-y1 != 0) ? ( (z3 - z1) / (double)(y3 - y1) ) : 0.0;
	double dz23 = (y3-y2 != 0) ? ( (z3 - z2) / (double)(y3 - y2) ) : 0.0;

	double du12 = (y2-y1 != 0) ? ( (u2 - u1) / (double)(y2 - y1) ) : 0.0;
	double du13 = (y3-y1 != 0) ? ( (u3 - u1) / (double)(y3 - y1) ) : 0.0;
	double du23 = (y3-y2 != 0) ? ( (u3 - u2) / (double)(y3 - y2) ) : 0.0;

	double dv12 = (y2-y1 != 0) ? ( (v2 - v1) / (double)(y2 - y1) ) : 0.0;
	double dv13 = (y3-y1 != 0) ? ( (v3 - v1) / (double)(y3 - y1) ) : 0.0;
	double dv23 = (y3-y2 != 0) ? ( (v3 - v2) / (double)(y3 - y2) ) : 0.0;

	double startx = x1, startz = z1, startu = u1, startv = v1;
	double stopx = x1, stopz = z1, stopu = u1, stopv = v1;

	for (int y = y1; y < y2; ++y)
	{
		texturedline(d, texture_pixelformat, twiddled, stride, texture, texture_xsize, texture_ysize, y, startx, stopx, startz, stopz, startu, stopu, startv, stopv);
		startx += dx13; startz += dz13; startu += du13; startv += dv13;
		stopx += dx12; stopz += dz12; stopu += du12; stopv += dv12;
	}

	stopx = x2; stopz = z2; stopu = u2; stopv = v2;
	for (int y = y2; y < y3; ++y)
	{
		texturedline(d, texture_pixelformat, twiddled, stride, texture, texture_xsize, texture_ysize, y, startx, stopx, startz, stopz, startu, stopu, startv, stopv);
		startx += dx13; startz += dz13; startu += du13; startv += dv13;
		stopx += dx23; stopz += dz23; stopu += du23; stopv += dv23;
	}

#ifdef DEBUG_RENDER_AS_WIRE_FRAME
	// Wire-frame test:
	line(d, x1, y1, x2, y2);
	line(d, x1, y1, x3, y3);
	line(d, x2, y2, x3, y3);
#endif
}


static void pvr_clear_ta_commands(struct pvr_data* d)
{
	d->n_ta_commands = 0;
}


/*
 *  pvr_render():
 *
 *  Render from the Object Buffer to the framebuffer.
 *
 *  TODO: This function is totally bogus so far, the format of the Object
 *        Buffer is just a quick made-up hack to see if it works at all.
 */
void pvr_render(struct cpu *cpu, struct pvr_data *d)
{
	int fb_render_cfg = REG(PVRREG_FB_RENDER_CFG);
	int fb_base = REG(PVRREG_FB_RENDER_ADDR1);

	if ((fb_render_cfg & FB_RENDER_CFG_RENDER_MODE_MASK) != 0x1) {
		printf("pvr: only RGB565 rendering has been implemented\n");
		exit(1);
	}

	// Settings for the current polygon being rendered:
	// Word 0:
	int listtype = 0;
	int striplength = 0;
	int clipmode;
	int modifier;
	int modifier_mode;
	int color_type = 0;
	bool texture = false;
	bool specular;
	bool shading;
	bool uv_format;

	// Word 1:
	int depthmode;
	int cullingmode = 0;
	bool zwrite;
	bool texture1;
	bool specular1;
	bool shading1;
	bool uv_format1;
	bool dcalcexact;

	// Word 2:
	int texture_usize = 0, texture_vsize = 0;

	// Word 3:
	bool texture_mipmap = false;
	bool texture_vq_compression = false;
	int texture_pixelformat = 0;
	bool texture_twiddled = false;
	bool texture_stride = false;
	uint32_t textureAddr = 0;

	int vertex_index = 0;
	int wf_x[4], wf_y[4]; double wf_z[4], wf_u[4], wf_v[4];
	int wf_r[4], wf_g[4], wf_b[4];

	double baseRed = 0.0, baseGreen = 0.0, baseBlue = 0.0;

	debug("[ pvr_render: rendering to FB offset 0x%x, "
	    "%i Tile Accelerator commands ]\n", fb_base, d->n_ta_commands);

	/*
	 *  Clear all pixels first.
	 *  TODO: Maybe only clear the specific tiles that are in use by
	 *  the tile accelerator?
	 *  TODO: What background color to use? See KOS' pvr_misc.c for
	 *  how KOS sets the background.
	 */
	memset(d->vram + fb_base, 0x00, d->xsize * d->ysize * d->bytes_per_pixel);

	/*  Clear Z as well:  */
	if (d->vram_z == NULL) {
		d->vram_z = (double*) malloc(sizeof(double) * d->xsize * d->ysize);
	}

	uint32_t bgplaneZ = REG(PVRREG_BGPLANE_Z);
	struct ieee_float_value backgroundz;
	ieee_interpret_float_value(bgplaneZ, &backgroundz, IEEE_FMT_S);
	for (int q = 0; q < d->xsize * d->ysize; ++q)
		d->vram_z[q] = backgroundz.f;

	// Using names from http://www.ludd.luth.se/~jlo/dc/ta-intro.txt.
	for (size_t index = 0; index < d->n_ta_commands; ++index) {
		// list points to 8 or 16 words.
		uint32_t* list = &d->ta_commands[index * 16];
		int cmd = (list[0] >> 29) & 7;

		switch (cmd)
		{
		case 0:	// END_OF_LIST
			// Interrupt event already triggered in pvr_ta_command().
#ifdef TA_DEBUG
			fatal("\nTA end_of_list (list type %i)\n", d->current_list_type);
#endif
			break;

		case 1:	// USER_CLIP
			// TODO: Ignoring for now.
			break;

		case 4:	// polygon or modifier volume
		{
			vertex_index = 0;

			// List Word 0:
			listtype = (list[0] >> 24) & 7;
			striplength = (list[0] >> 18) & 3;
			striplength = striplength == 2 ? 4 : (
			    striplength == 3 ? 6 : (striplength + 1));
			clipmode = (list[0] >> 16) & 3;
			modifier = (list[0] >> 7) & 1;
			modifier_mode = (list[0] >> 6) & 1;
			color_type = (list[0] >> 4) & 3;
			texture = list[0] & 8;
			specular = list[0] & 4;
			shading = list[0] & 2;
			uv_format = list[0] & 1;

#ifdef TA_DEBUG
			fatal("\nTA polygon  listtype %i, ", listtype);
			fatal("striplength %i, ", striplength);
			fatal("clipmode %i, ", clipmode);
			fatal("modifier %i, ", modifier);
			fatal("modifier_mode %i,\n", modifier_mode);
			fatal("            color_type %i, ", color_type);
			fatal("texture %s, ", texture ? "TRUE" : "false");
			fatal("specular %s, ", specular ? "TRUE" : "false");
			fatal("shading %s, ", shading ? "TRUE" : "false");
			fatal("uv_format %s\n", uv_format ? "TRUE" : "false");
#endif

			// List Word 1:
			depthmode = (list[1] >> 29) & 7;
			cullingmode = (list[1] >> 27) & 3;
			zwrite = ! ((list[1] >> 26) & 1);
			texture1 = (list[1] >> 25) & 1;
			specular1 = (list[1] >> 24) & 1;
			shading1 = (list[1] >> 23) & 1;
			uv_format1 = (list[1] >> 22) & 1;
			dcalcexact = (list[1] >> 20) & 1;

#ifdef TA_DEBUG
			fatal("            depthmode %i, ", depthmode);
			fatal("cullingmode %i, ", cullingmode);
			fatal("zwrite %s, ", zwrite ? "TRUE" : "false");
			fatal("texture1 %s\n", texture1 ? "TRUE" : "false");
			fatal("            specular1 %s, ", specular1 ? "TRUE" : "false");
			fatal("shading1 %s, ", shading1 ? "TRUE" : "false");
			fatal("uv_format1 %s, ", uv_format1 ? "TRUE" : "false");
			fatal("dcalcexact %s\n", dcalcexact ? "TRUE" : "false");
#endif

			if (!zwrite) {
				fatal("pvr: no zwrite? not implemented yet.\n");
				exit(1);
			}

			// For now, trust texture and ignore texture1.
			// if (texture != texture1) {
			//	fatal("pvr: texture != texture1. what to do?\n");
			//	exit(1);
			// }

			// List Word 2:
			// TODO: srcblend (31-29)
			// TODO: dstblend (28-26)
			// TODO: srcmode (25)
			// TODO: dstmode (24)
			// TODO: fog (23-22)
			// TODO: clamp (21)
			// TODO: alpha (20)
			// TODO: texture alpha (19)
			// TODO: uv flip (18-17)
			// TODO: uv clamp (16-15)
			// TODO: filter (14-12)
			// TODO: mipmap (11-8)
			// TODO: texture shading (7-6)
			texture_usize = 8 << ((list[2] >> 3) & 7);
			texture_vsize = 8 << (list[2] & 7);

			// List Word 3:
			texture_mipmap = (list[3] >> 31) & 1;
			texture_vq_compression = (list[3] >> 30) & 1;
			texture_pixelformat = (list[3] >> 27) & 7;
			texture_twiddled = ! ((list[3] >> 26) & 1);
			texture_stride = (list[3] >> 25) & 1;
			textureAddr = (list[3] << 3) & 0x7fffff;

#ifdef TA_DEBUG
			fatal("            texture: mipmap %s, ", texture_mipmap ? "TRUE" : "false");
			fatal("vq_compression %s, ", texture_vq_compression ? "TRUE" : "false");
			fatal("pixelformat %i, ", texture_pixelformat);
			fatal("twiddled %s\n", texture_twiddled ? "TRUE" : "false");
			fatal("            stride %s, ", texture_stride ? "TRUE" : "false");
			fatal("textureAddr 0x%08x\n", textureAddr);
#endif

			if (texture_vq_compression) {
				fatal("pvr: texture_vq_compression not supported yet\n");
				// exit(1);
			}

			struct ieee_float_value r, g, b;
			ieee_interpret_float_value(list[5], &r, IEEE_FMT_S);
			ieee_interpret_float_value(list[6], &g, IEEE_FMT_S);
			ieee_interpret_float_value(list[7], &b, IEEE_FMT_S);
			baseRed = r.f * 255;
			baseGreen = g.f * 255;
			baseBlue = b.f * 255;
			// printf("rgb = %f %f %f\n", r.f, g.f, b.f);
			break;
		}

		case 7:	// vertex
		{
			// MAJOR TODO:
			// How to select which one of the 18 (!) types listed
			// in http://www.ludd.luth.se/~jlo/dc/ta-intro.txt to
			// use?
			if (listtype != 0 && listtype != 2 && listtype != 4)
				break;

			bool eos = (list[0] >> 28) & 1;

			struct ieee_float_value fx, fy, fz, u, v, extra1, extra2;
			ieee_interpret_float_value(list[1], &fx, IEEE_FMT_S);
			ieee_interpret_float_value(list[2], &fy, IEEE_FMT_S);
			ieee_interpret_float_value(list[3], &fz, IEEE_FMT_S);
			wf_x[vertex_index] = fx.f;
			wf_y[vertex_index] = fy.f;
			wf_z[vertex_index] = fz.f;

#ifdef TA_DEBUG
			fatal("TA vertex   %f %f %f%s\n", fx.f, fy.f, fz.f,
				eos ? " end_of_strip" : "");
#endif

			if (texture) {
				ieee_interpret_float_value(list[4], &u, IEEE_FMT_S);
				ieee_interpret_float_value(list[5], &v, IEEE_FMT_S);
				wf_u[vertex_index] = u.f;
				wf_v[vertex_index] = v.f;
			} else {
				if (color_type == 0) {
					wf_r[vertex_index] = (list[6] >> 16) & 255;
					wf_g[vertex_index] = (list[6] >> 8) & 255;
					wf_b[vertex_index] = (list[6]) & 255;
				} else if (color_type == 1) {
					ieee_interpret_float_value(list[5], &v, IEEE_FMT_S);
					ieee_interpret_float_value(list[6], &extra1, IEEE_FMT_S);
					ieee_interpret_float_value(list[7], &extra2, IEEE_FMT_S);
					wf_r[vertex_index] = v.f * 255;
					wf_g[vertex_index] = extra1.f * 255;
					wf_b[vertex_index] = extra2.f * 255;
				} else if (color_type == 2) {
					ieee_interpret_float_value(list[6], &extra1, IEEE_FMT_S);
					wf_r[vertex_index] = extra1.f * baseRed;
					wf_g[vertex_index] = extra1.f * baseGreen;
					wf_b[vertex_index] = extra1.f * baseBlue;
				} else {
					// "Intensity from previous face". TODO. Red for now.
					wf_r[vertex_index] = 255;
					wf_g[vertex_index] = 0;
					wf_b[vertex_index] = 0;
				}
			}

			vertex_index ++;

			if (vertex_index >= 3) {
				int modulo_mask = REG(PVRREG_TSP_CFG) & TSP_CFG_MODULO_MASK;

				float crossProduct =
					((wf_x[1] - wf_x[0])*(wf_y[2] - wf_y[0])) -
					((wf_y[1] - wf_y[0])*(wf_x[2] - wf_x[0]));

				// Hm. TODO: Instead of flipping back and forth between
				// clockwise and counter-clockwise culling, perhaps there
				// is some smarter way of assigning the three points
				// instead of 012 => 12x...?
				bool culled = false;
				if (cullingmode == 2) {
					if (crossProduct < 0)
						culled = true;
					cullingmode = 3;
				} else if (cullingmode == 3) {
					if (crossProduct > 0)
						culled = true;
					cullingmode = 2;
				}

				if (!culled) {
					if (texture)
						pvr_render_triangle_textured(d,
						    texture_pixelformat, texture_twiddled, texture_stride ? (32*modulo_mask) : 0,
						    textureAddr, texture_usize, texture_vsize,
						    wf_x[0], wf_y[0], wf_z[0], wf_u[0], wf_v[0],
						    wf_x[1], wf_y[1], wf_z[1], wf_u[1], wf_v[1],
						    wf_x[2], wf_y[2], wf_z[2], wf_u[2], wf_v[2]);
					else
						pvr_render_triangle(d,
						    wf_x[0], wf_y[0], wf_z[0], wf_r[0], wf_g[0], wf_b[0],
						    wf_x[1], wf_y[1], wf_z[1], wf_r[1], wf_g[1], wf_b[1],
						    wf_x[2], wf_y[2], wf_z[2], wf_r[2], wf_g[2], wf_b[2]);
				}

				if (eos) {
					// End of strip.
					vertex_index = 0;
				} else {
					// Not a closing vertex, then move points 1 and 2
					// into slots 0 and 1, so that the stripe can continue.
					vertex_index = 2;
					wf_x[0] = wf_x[1]; wf_y[0] = wf_y[1]; wf_z[0] = wf_z[1];
					wf_u[0] = wf_u[1]; wf_v[0] = wf_v[1];
					wf_r[0] = wf_r[1]; wf_g[0] = wf_g[1]; wf_b[0] = wf_b[1];
					
					wf_x[1] = wf_x[2]; wf_y[1] = wf_y[2]; wf_z[1] = wf_z[2];
					wf_u[1] = wf_u[2]; wf_v[1] = wf_v[2];
					wf_r[1] = wf_r[2]; wf_g[1] = wf_g[2]; wf_b[1] = wf_b[2];
				}
			}
			break;
		}

		default:
			fatal("pvr_render: unimplemented list cmd %i\n", cmd);
			exit(1);
		}
	}

	pvr_clear_ta_commands(d);
	
	// TODO: RENDERDONE is 2. How about other events?
	SYSASIC_TRIGGER_EVENT(SYSASIC_EVENT_RENDERDONE);
}


/*
 *  pvr_reset_ta():
 *
 *  Reset the Tile Accelerator.
 */
static void pvr_reset_ta(struct pvr_data *d)
{
	REG(PVRREG_DIWCONF) = DIWCONF_MAGIC;
	pvr_clear_ta_commands(d);
}


/*
 *  pvr_reset():
 *
 *  Reset the PVR.
 */
static void pvr_reset(struct pvr_data *d)
{
	/*  TODO  */
}


/*
 *  pvr_ta_init():
 *
 *  Initialize the Tile Accelerator. This makes the TA ready to receive
 *  commands (via address 0x10000000).
 */
void pvr_ta_init(struct cpu *cpu, struct pvr_data *d)
{
	REG(PVRREG_TA_OPB_POS) = REG(PVRREG_TA_OPB_START);
	REG(PVRREG_TA_OB_POS) = REG(PVRREG_TA_OB_START);
}


static void pvr_tilebuf_debugdump(struct pvr_data *d)
{
	return;

	// According to Marcus Comstedt's "tatest":
	// 24 word header (before the TILEBUF_ADDR pointer), followed by
	// 6 words for each tile.
	uint32_t tilebuf = REG(PVRREG_TILEBUF_ADDR) & PVR_TILEBUF_ADDR_MASK;
	uint32_t *p = (uint32_t*) (d->vram + tilebuf);

	fatal("PVR tile buffer debug dump:\n");
	p -= 24;
	for (int i = 0; i < 24; ++i)
		fatal("  %08x", *p++);

	fatal("\n%i x %i tiles:\n", d->tilebuf_xsize, d->tilebuf_ysize);

	for (int x = 0; x < d->tilebuf_xsize; ++x)
	{
		for (int y = 0; y < d->tilebuf_ysize; ++y)
		{
			fatal("  Tile %i,%i:", x, y);
			for (int i = 0; i < 6; ++i)
				fatal(" %08x", *p++);
			fatal("\n");
		}
	}
}


/*
 *  pvr_ta_command():
 *
 *  Someone has written a [complete] 32-byte or 64-byte command to the Tile
 *  Accelerator memory area. The real hardware probably outputs
 *  "compiled commands" into the Object list and Object Pointer list.
 *  For now, just put all the commands in a plain array, and then later execute
 *  them in pvr_render().
 */
static void pvr_ta_command(struct cpu *cpu, struct pvr_data *d, int list_ofs)
{
	uint32_t *ta = &d->ta[list_ofs];

#ifdef TA_DEBUG
	/*  Dump the Tile Accelerator command for debugging:  */
	{
		int i;
		fatal("TA cmd:");
		for (i = 0; i < 8; ++i)
			fatal(" %08x", (int) ta[i]);
		fatal("\n");
	}
#endif

	// ob_ofs = REG(PVRREG_TA_OB_POS);
	// REG(PVRREG_TA_OB_POS) = ob_ofs + sizeof(uint64_t);

	if (d->ta_commands == NULL) {
		d->allocated_ta_commands = 2048;
		d->ta_commands = (uint32_t *) malloc(64 * d->allocated_ta_commands);
		d->n_ta_commands = 0;
	}

	if (d->n_ta_commands + 1 >= d->allocated_ta_commands) {
		d->allocated_ta_commands *= 2;
		d->ta_commands = (uint32_t *) realloc(d->ta_commands, 64 * d->allocated_ta_commands);
	}

	// Hack: I don't understand yet what separates a 32-byte transfer
	// vs two individual 32-byte transfers vs a 64-byte transfer.
	// TODO: For now, really only support 32-byte transfers... :(
	memcpy(d->ta_commands + 16 * d->n_ta_commands, ta, 32);
	memset(d->ta_commands + 16 * d->n_ta_commands + 8, 0, 32);
	d->n_ta_commands ++;

	// We need to keep track of the current list type though, and respond
	// with an event once we reach an end_of_list command. All other
	// commands are handled in pvr_render() for now.
	int cmd = (ta[0] >> 29) & 7;
	if (cmd == 0) {
		// cmd 0: end of list
		uint32_t opb_cfg = REG(PVRREG_TA_OPB_CFG);

		if (d->current_list_type == 0 && opb_cfg & TA_OPB_CFG_OPAQUEPOLY_MASK)
			SYSASIC_TRIGGER_EVENT(SYSASIC_EVENT_OPAQUEDONE);
		if (d->current_list_type == 1 && opb_cfg & TA_OPB_CFG_OPAQUEMOD_MASK)
			SYSASIC_TRIGGER_EVENT(SYSASIC_EVENT_OPAQUEMODDONE);
		if (d->current_list_type == 2 && opb_cfg & TA_OPB_CFG_TRANSPOLY_MASK)
			SYSASIC_TRIGGER_EVENT(SYSASIC_EVENT_TRANSDONE);
		if (d->current_list_type == 3 && opb_cfg & TA_OPB_CFG_TRANSMOD_MASK)
			SYSASIC_TRIGGER_EVENT(SYSASIC_EVENT_TRANSMODDONE);
		if (d->current_list_type == 4 && opb_cfg & TA_OPB_CFG_PUNCHTHROUGH_MASK)
			SYSASIC_TRIGGER_EVENT(SYSASIC_EVENT_PVR_PTDONE);
	} else if (cmd == 4) {
		// cmd 4: polygon or modifier volume
		d->current_list_type = (ta[0] >> 24) & 7;
	}
}


DEVICE_ACCESS(pvr_ta)
{
	struct pvr_data *d = (struct pvr_data *) extra;
	uint64_t idata = 0, odata = 0;

	if (len != sizeof(uint32_t)) {
		fatal("pvr_ta access len = %i: TODO\n", (int) len);
		exit(1);
	}

	// Tile Accelerator commands can be sent to 0x10000000 through
	// 0x107fffff, it seems, but the SH4 store queues only have 64 bytes.
	relative_addr &= (sizeof(d->ta) - 1);

	if (writeflag == MEM_WRITE) {
		idata = memory_readmax64(cpu, data, len);
#if 0
		fatal("[ pvr_ta: WRITE addr=%08x value=%08x ]\n",
		    (int)relative_addr, (int)idata);
#endif

		/*  Write to the tile accelerator command buffer:  */
		d->ta[relative_addr / sizeof(uint32_t)] = idata;

		// Execute the command, after a complete write.
		// (Note: This assumes that commands are written from low
		// address to high.)
		if (relative_addr == 0x1c)
			pvr_ta_command(cpu, d, 0);
		if (relative_addr == 0x3c)
			pvr_ta_command(cpu, d, 8);
	} else {
		odata = d->ta[relative_addr / sizeof(uint32_t)];
		memory_writemax64(cpu, data, len, odata);
#if 1
		fatal("[ pvr_ta: READ addr=%08x value=%08x ]\n", (int)relative_addr, (int)odata);
#endif
	}

	return 1;
}


DEVICE_ACCESS(pvr)
{
	struct pvr_data *d = (struct pvr_data *) extra;
	uint64_t idata = 0, odata = 0;

	if (writeflag == MEM_WRITE)
		idata = memory_readmax64(cpu, data, len);

	/*  Default read action: Read from reg[]:  */
	if (writeflag == MEM_READ)
		odata = d->reg[relative_addr / sizeof(uint32_t)];

	/*  Fog table access:  */
	if (relative_addr >= PVRREG_FOG_TABLE &&
	    relative_addr < PVRREG_FOG_TABLE + PVR_FOG_TABLE_SIZE) {
		if (writeflag == MEM_WRITE)
			DEFAULT_WRITE;
		goto return_ok;
	}

	/*  Palette access:  */
	if (relative_addr >= PVRREG_PALETTE &&
	    relative_addr < PVRREG_PALETTE + PVR_PALETTE_SIZE) {
		if (writeflag == MEM_WRITE)
			DEFAULT_WRITE;
		goto return_ok;
	}

	switch (relative_addr) {

	case PVRREG_ID:
		/*  ID for Set 5.xx versions of the Dreamcast, according
		    to http://www.ludd.luth.se/~jlo/dc/powervr-reg.txt:  */
		odata = 0x17fd11db;
		break;

	case PVRREG_REVISION:
		/*  Revision 1.1, for Dreamcast Set 5.2x.  */
		odata = 0x00000011;
		break;

	case PVRREG_RESET:
		if (writeflag == MEM_WRITE) {
			if (idata != 0) {
				debug("[ pvr: RESET ");
				if (idata & PVR_RESET_PVR)
					pvr_reset(d);
				if (idata & PVR_RESET_TA)
					pvr_reset_ta(d);
				debug("]\n");
			}
			idata = 0;
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_STARTRENDER:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: STARTRENDER ]\n");
			pvr_render(cpu, d);
		} else {
			fatal("[ pvr: huh? read from STARTRENDER ]\n");
			exit(1);
		}
		break;

	case PVRREG_OB_ADDR:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: OB_ADDR set to 0x%08"PRIx32" ]\n",
			    (uint32_t)(idata & PVR_OB_ADDR_MASK));
			if (idata & ~PVR_OB_ADDR_MASK) {
				fatal("[ pvr: OB_ADDR: Fatal error: Unknown"
				    " bits set: 0x%08"PRIx32" ]\n",
				    (uint32_t)(idata & ~PVR_OB_ADDR_MASK));
				exit(1);
			}
			idata &= PVR_OB_ADDR_MASK;
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_TILEBUF_ADDR:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: TILEBUF_ADDR set to 0x%08"PRIx32" ]\n",
			    (uint32_t)(idata & PVR_TILEBUF_ADDR_MASK));
			if (idata & ~PVR_TILEBUF_ADDR_MASK) {
				fatal("[ pvr: TILEBUF_ADDR: Unknown"
				    " bits set: 0x%08"PRIx32" ]\n",
				    (uint32_t)(idata & ~PVR_TILEBUF_ADDR_MASK));
				exit(1);
			}
			idata &= PVR_TILEBUF_ADDR_MASK;
			DEFAULT_WRITE;
			pvr_tilebuf_debugdump(d);
		}
		break;

	case PVRREG_SPANSORT:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: SPANSORT: ");
			if (idata & PVR_SPANSORT_SPAN0)
				debug("SPAN0 ");
			if (idata & PVR_SPANSORT_SPAN1)
				debug("SPAN1 ");
			if (idata & PVR_SPANSORT_TSP_CACHE_ENABLE)
				debug("TSP_CACHE_ENABLE ");
			debug("]\n");
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_BRDCOLR:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: BRDCOLR set to 0x%06"PRIx32" ]\n",
			    (int)idata);
			DEFAULT_WRITE;
			d->border_updated = 1;
		}
		break;

	case PVRREG_DIWMODE:
		if (writeflag == MEM_WRITE) {
			d->clock_double = idata & DIWMODE_C_MASK? 1:0;
			d->strip_buffer_enabled = idata & DIWMODE_SE_MASK? 1:0;
			d->strip_length = (idata & DIWMODE_SL_MASK)
			    >> DIWMODE_SL_SHIFT;
			d->argb8888_threshold = (idata & DIWMODE_TH_MASK)
			    >> DIWMODE_TH_SHIFT;
			d->extend = (idata & DIWMODE_EX_MASK)
			    >> DIWMODE_EX_SHIFT;
			d->pixelmode = (idata & DIWMODE_COL_MASK)
			    >> DIWMODE_COL_SHIFT;
			d->line_double = idata & DIWMODE_SD_MASK? 1:0;
			d->display_enabled = idata & DIWMODE_DE_MASK? 1:0;

			debug("[ pvr: DIWMODE set to: ");
			debug("clock_double=%i, ", d->clock_double);
			debug("strip_buffer_enabled=%i, ",
			    d->strip_buffer_enabled);
			debug("strip_length=%i, ", d->strip_length);
			debug("argb8888_threshold=%i, ", d->argb8888_threshold);
			debug("extend=0x%x, ", d->extend);
			debug("pixelmode=");
			switch (d->pixelmode) {
			case 0: debug("RGB0555 (16-bit)"); break;
			case 1: debug("RGB565 (16-bit)"); break;
			case 2: debug("RGB888 (24-bit)"); break;
			case 3: debug("RGB0888 (32-bit)"); break;
			}
			debug(", line_double=%i, ", d->line_double);
			debug("display_enabled=%i", d->display_enabled);
			debug(" ]\n");

			DEFAULT_WRITE;
			pvr_geometry_updated(d);
			pvr_fb_invalidate(d, -1, -1);
		}
		break;

	case PVRREG_DIWSIZE:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: DIWSIZE set to modulo=%i, "
			    "width=%i, height=%i ]\n", (int)
			    ((idata >> DIWSIZE_MODULO_SHIFT) & DIWSIZE_MASK),
			    (int)((idata >> DIWSIZE_DPL_SHIFT) & DIWSIZE_MASK),
			    (int)((idata >> DIWSIZE_LPF_SHIFT) & DIWSIZE_MASK));
			DEFAULT_WRITE;
			pvr_geometry_updated(d);
			pvr_fb_invalidate(d, -1, -1);
		}
		break;

	case PVRREG_FB_RENDER_ADDR1:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: FB_RENDER_ADDR1 set to 0x%08"PRIx32
			    " ]\n", (int) idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_FB_RENDER_ADDR2:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: FB_RENDER_ADDR2 set to 0x%08"PRIx32
			    " ]\n", (int) idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_FB_CLIP_X:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: FB_CLIP_X set to min=%i, "
			    "max=%i ]\n", (int) (idata & FB_CLIP_XY_MIN_MASK),
			    (int) ((idata & FB_CLIP_XY_MAX_MASK)
			    >> FB_CLIP_XY_MAX_SHIFT));
			DEFAULT_WRITE;
			pvr_geometry_updated(d);
			pvr_fb_invalidate(d, -1, -1);
		}
		break;

	case PVRREG_FB_CLIP_Y:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: FB_CLIP_Y set to min=%i, "
			    "max=%i ]\n", (int) (idata & FB_CLIP_XY_MIN_MASK),
			    (int) ((idata & FB_CLIP_XY_MAX_MASK)
			    >> FB_CLIP_XY_MAX_SHIFT));
			DEFAULT_WRITE;
			pvr_geometry_updated(d);
			pvr_fb_invalidate(d, -1, -1);
		}
		break;

	case PVRREG_SHADOW:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: SHADOW set to enable=%i, "
			    "intensity=%i ]\n",
			    (int) (idata & SHADOW_ENABLE? 1 : 0),
			    (int) (idata & SHADOW_INTENSITY_MASK));
			DEFAULT_WRITE;
			pvr_geometry_updated(d);
			pvr_fb_invalidate(d, -1, -1);
		}
		break;

	case PVRREG_OBJECT_CLIP:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: OBJECT_CLIP 0x%08x ]\n", (int)idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_OB_CFG:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: OB_CFG 0x%08x ]\n", (int)idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_UNKNOWN_80:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: UNKNOWN_80 0x%08x ]\n", (int)idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_UNKNOWN_84:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: UNKNOWN_84 0x%08x ]\n", (int)idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_BGPLANE_Z:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: BGPLANE_Z 0x%08x ]\n", (int)idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_BGPLANE_CFG:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: BGPLANE_CFG 0x%08x ]\n", (int)idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_ISP_CFG:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: ISP_CFG 0x%08x ]\n", (int)idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_VRAM_CFG1:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: VRAM_CFG1 set to 0x%08"PRIx32,
			    (int) idata);
			if (idata != VRAM_CFG1_GOOD_REFRESH_VALUE)
				fatal("{ VRAM_CFG1 = 0x%08"PRIx32" is not "
				    "yet implemented! }", (int) idata);
			debug(" ]\n");
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_VRAM_CFG2:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: VRAM_CFG2 set to 0x%08"PRIx32,
			    (int) idata);
			if (idata != VRAM_CFG2_UNKNOWN_MAGIC)
				fatal("{ VRAM_CFG2 = 0x%08"PRIx32" is not "
				    "yet implemented! }", (int) idata);
			debug(" ]\n");
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_VRAM_CFG3:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: VRAM_CFG3 set to 0x%08"PRIx32,
			    (int) idata);
			if (idata != VRAM_CFG3_UNKNOWN_MAGIC)
				fatal("{ VRAM_CFG3 = 0x%08"PRIx32" is not "
				    "yet implemented! }", (int) idata);
			debug(" ]\n");
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_FOG_TABLE_COL:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: FOG_TABLE_COL set to 0x%06"PRIx32" ]\n",
			    (int) idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_FOG_VERTEX_COL:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: FOG_VERTEX_COL set to 0x%06"PRIx32" ]\n",
			    (int) idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_FOG_DENSITY:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: FOG_DENSITY set to 0x%08"PRIx32" ]\n",
			    (int) idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_CLAMP_MAX:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: CLAMP_MAX set to 0x%06"PRIx32" ]\n",
			    (int) idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_CLAMP_MIN:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: CLAMP_MIN set to 0x%06"PRIx32" ]\n",
			    (int) idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_FB_RENDER_CFG:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: PVRREG_FB_RENDER_CFG set to 0x%08x ]\n",
			    (int) idata);
			/*  TODO  */
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_FB_RENDER_MODULO:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: PVRREG_FB_RENDER_MODULO set to %i ]\n",
			    (int) idata);
			/*  TODO  */
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_DIWADDRL:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: DIWADDRL set to 0x%08"PRIx32" ]\n",
			    (int) idata);
			pvr_fb_invalidate(d, -1, -1);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_DIWADDRS:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: DIWADDRS set to 0x%08"PRIx32" ]\n",
			    (int) idata);
			pvr_fb_invalidate(d, -1, -1);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_HPOS_IRQ:
		DEFAULT_WRITE;
		break;

	case PVRREG_RASEVTPOS:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: RASEVTPOS pos1=%i pos2=%i ]\n",
			    (int)((idata & RASEVTPOS_POS1_MASK)
			    >> RASEVTPOS_POS1_SHIFT),
			    (int)(idata & RASEVTPOS_POS2_MASK));
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_SYNCCONF:
		if (writeflag == MEM_WRITE) {
			d->video_enabled = idata & SYNCCONF_VO_MASK? 1:0;
			d->broadcast_standard = (idata & SYNCCONF_BC_MASK)
			    >> SYNCCONF_BC_SHIFT;
			d->interlaced = idata & SYNCCONF_I_MASK? 1:0;
			d->h_sync_positive = idata & SYNCCONF_HP_MASK? 1:0;
			d->v_sync_positive = idata & SYNCCONF_VP_MASK? 1:0;

			debug("[ pvr: SYNCCONF set to: ");
			debug("video_enabled=%i, ", d->video_enabled);
			switch (d->broadcast_standard) {
			case SYNCCONF_BC_VGA: debug("VGA"); break;
			case SYNCCONF_BC_NTSC: debug("NTSC"); break;
			case SYNCCONF_BC_PAL: debug("PAL"); break;
			default: debug("*UNKNOWN*"); break;
			}
			debug(", interlaced=%i, ", d->interlaced);
			debug("hsync=%i, ", d->h_sync_positive);
			debug("vsync=%i ]\n", d->v_sync_positive);

			DEFAULT_WRITE;
			pvr_geometry_updated(d);
			pvr_fb_invalidate(d, -1, -1);
		}
		break;

	case PVRREG_BRDHORZ:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: BRDHORZ start=%i stop=%i ]\n",
			    (int)((idata & BRDHORZ_START_MASK)
			    >> BRDHORZ_START_SHIFT),
			    (int)(idata & BRDHORZ_STOP_MASK));
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_SYNCSIZE:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: SYNCSIZE v=%i h=%i ]\n",
			    (int)((idata & SYNCSIZE_V_MASK)
			    >> SYNCSIZE_V_SHIFT),
			    (int)(idata & SYNCSIZE_H_MASK));
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_BRDVERT:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: BRDVERT start=%i stop=%i ]\n",
			    (int)((idata & BRDVERT_START_MASK)
			    >> BRDVERT_START_SHIFT),
			    (int)(idata & BRDVERT_STOP_MASK));
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_SYNCH_WIDTH:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: SYNCH_WIDTH 0x%08x ]\n", (int)idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_TSP_CFG:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: TSP_CFG 0x%08x ]\n", (int)idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_DIWCONF:
		if (writeflag == MEM_WRITE) {
			if ((idata & DIWCONF_MAGIC_MASK) !=
			    DIWCONF_MAGIC && (idata & DIWCONF_MAGIC_MASK)
			    != 0) {
				fatal("PVRREG_DIWCONF magic not set to "
				    "Magic value. 0x%08x\n", (int)idata);
				exit(1);
			}
			if (idata & DIWCONF_BLANK)
				debug("[ pvr: PVRREG_DIWCONF: BLANK: TODO ]\n");

			DEFAULT_WRITE;
			pvr_geometry_updated(d);
		}
		break;

	case PVRREG_DIWHSTRT:
		if (writeflag == MEM_WRITE) {
			int v = idata & DIWVSTRT_HPOS_MASK;
			debug("[ pvr: DIWHSTRT hpos=%i (%s) ]\n",
			    v, v == 174? "PAL" :
			    (v == 164? "NTSC" :
			    (v == 144? "VGA" : "unknown!")));
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_DIWVSTRT:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: DIWVSTRT v2=%i v1=%i ]\n",
			    (int)((idata & DIWVSTRT_V2_MASK)
			    >> DIWVSTRT_V2_SHIFT),
			    (int)(idata & DIWVSTRT_V1_MASK));
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_SCALER_CFG:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: SCALER_CFG 0x%08x ]\n", (int)idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_PALETTE_CFG:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: PALETTE_CFG 0x%08x ]\n", (int)idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_SYNC_STAT:
		/*  TODO. Ugly hack, but it works:  */
		odata = random();
		break;

	case PVRREG_MAGIC_110:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: MAGIC_110 set to 0x%08"PRIx32,
			    (int) idata);
			if (idata != MAGIC_110_VALUE)
				fatal("{ MAGIC_110 = 0x%08"PRIx32" is not "
				    "yet implemented! }", (int) idata);
			debug(" ]\n");
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_TA_LUMINANCE:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: TA_LUMINANCE set to 0x%08"PRIx32" ]\n",
			    (int) idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_TA_OPB_START:
		if (writeflag == MEM_WRITE) {
			if (idata & ~TA_OPB_START_MASK) {
				fatal("[ pvr: UNEXPECTED bits in "
				    "TA_OPB_START: 0x%08x ]\n", (int)idata);
				exit(1);
			}
			idata &= TA_OPB_START_MASK;
			debug("[ pvr: TA_OPB_START set to 0x%x ]\n",
			    (int) idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_TA_OB_START:
		if (writeflag == MEM_WRITE) {
			if (idata & ~TA_OB_START_MASK) {
				fatal("[ pvr: UNEXPECTED bits in "
				    "TA_OB_START: 0x%08x ]\n", (int)idata);
				exit(1);
			}
			idata &= TA_OB_START_MASK;
			debug("[ pvr: TA_OB_START set to 0x%x ]\n",
			    (int) idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_TA_OPB_END:
		if (writeflag == MEM_WRITE) {
			idata &= TA_OPB_END_MASK;
			debug("[ pvr: TA_OPB_END set to 0x%x ]\n",
			    (int) idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_TA_OB_END:
		if (writeflag == MEM_WRITE) {
			idata &= TA_OB_END_MASK;
			debug("[ pvr: TA_OB_END set to 0x%x ]\n",
			    (int) idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_TA_OPB_POS:
		if (writeflag == MEM_WRITE) {
			idata &= TA_OPB_POS_MASK;
			debug("[ pvr: TA_OPB_POS set to 0x%x ]\n",
			    (int) idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_TA_OB_POS:
		if (writeflag == MEM_WRITE) {
			idata &= TA_OB_POS_MASK;
			debug("[ pvr: TA_OB_POS set to 0x%x ]\n",
			    (int) idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_TA_OPL_INIT:
		if (writeflag == MEM_WRITE) {
			idata &= PVR_TA_OPL_INIT_MASK;
			debug("[ pvr: TA_OPL_INIT set to 0x%x ]\n",
			    (int) idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_TILEBUF_SIZE:
		if (writeflag == MEM_WRITE) {
			d->tilebuf_ysize = (idata & TILEBUF_SIZE_HEIGHT_MASK)
			    >> TILEBUF_SIZE_HEIGHT_SHIFT;
			d->tilebuf_xsize = idata & TILEBUF_SIZE_WIDTH_MASK;
			d->tilebuf_xsize ++; d->tilebuf_ysize ++;
			debug("[ pvr: TILEBUF_SIZE set to %i x %i ]\n",
			    d->tilebuf_xsize, d->tilebuf_ysize);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_TA_OPB_CFG:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: TA_OPB_CFG set to 0x%x ]\n",
			    (int) idata);
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_TA_INIT:
		if (writeflag == MEM_WRITE) {
			debug("[ pvr: TA_INIT ]\n");

			if (idata & PVR_TA_INIT)
				pvr_ta_init(cpu, d);

			if (idata != PVR_TA_INIT && idata != 0)
				fatal("{ TA_INIT = 0x%08"PRIx32" is not "
				    "yet implemented! }", (int) idata);

			/*  Always reset to 0.  */
			idata = 0;
			DEFAULT_WRITE;
		}
		break;

	case PVRREG_YUV_STAT:
		// TODO. The "luftvarg" demo accesses this register.
		break;

	default:if (writeflag == MEM_READ) {
			fatal("[ pvr: read from UNIMPLEMENTED addr 0x%x ]\n",
			    (int)relative_addr);
		} else {
			fatal("[ pvr: write to UNIMPLEMENTED addr 0x%x: 0x%x"
			    " ]\n", (int)relative_addr, (int)idata);
			DEFAULT_WRITE;
		}

		exit(1);
	}

return_ok:
	if (writeflag == MEM_READ)
		memory_writemax64(cpu, data, len, odata);

	return 1;
}


void pvr_extend_update_region(struct pvr_data *d, uint64_t low, uint64_t high)
{
	int vram_ofs = REG(PVRREG_DIWADDRL);
	int bytes_per_line = d->xsize * d->bytes_per_pixel;

	low -= vram_ofs;
	high -= vram_ofs;

	/*  Access inside visible part of VRAM?  */
	if ((int64_t)high >= 0 && (int64_t)low <
	    bytes_per_line * d->ysize) {
		int new_y1, new_y2;

		d->fb_update_x1 = 0;
		d->fb_update_x2 = d->xsize - 1;

		/*  Calculate which line the low and high addresses
		    correspond to:  */
		new_y1 = low / bytes_per_line;
		new_y2 = high / bytes_per_line + 1;

		if (d->fb_update_y1 < 0 || new_y1 < d->fb_update_y1)
			d->fb_update_y1 = new_y1;
		if (d->fb_update_y2 < 0 || new_y2 > d->fb_update_y2)
			d->fb_update_y2 = new_y2;

		if (d->fb_update_y1 < 0)
			d->fb_update_y1 = 0;
		if (d->fb_update_y2 >= d->ysize)
			d->fb_update_y2 = d->ysize - 1;
	}
}


DEVICE_TICK(pvr_fb)
{
	struct pvr_data *d = (struct pvr_data *) extra;
	uint64_t high, low = (uint64_t)(int64_t) -1;
	int vram_ofs = REG(PVRREG_DIWADDRL), pixels_to_copy;
	int bytes_per_line = d->xsize * d->bytes_per_pixel;
	int fb_ofs, p;
	uint8_t *fb = (uint8_t *) d->fb->framebuffer;
	uint8_t *vram = (uint8_t *) d->vram;


	/*
	 *  Vertical retrace interrupts:
	 *
	 *  TODO: Maybe it would be even more realistic to have the timer run
	 *        at, say, 60*4 = 240 Hz, and have the following events:
	 *
	 *	  (tick & 3) == 0	SYSASIC_EVENT_VBLINT
	 *	  (tick & 3) == 1	SYSASIC_EVENT_PVR_SCANINT1
	 *	  (tick & 3) == 2	nothing
	 *	  (tick & 3) == 3	SYSASIC_EVENT_PVR_SCANINT2
	 */
	if (d->vblank_interrupts_pending > 0) {
		-- d->vblank_interrupts_pending;

		SYSASIC_TRIGGER_EVENT(SYSASIC_EVENT_VBLINT);
		SYSASIC_TRIGGER_EVENT(SYSASIC_EVENT_PVR_SCANINT1);
		
		// Is this needed?
		SYSASIC_TRIGGER_EVENT(SYSASIC_EVENT_PVR_SCANINT2);

		/*  TODO: For now, I don't care about missed interrupts:  */
		d->vblank_interrupts_pending = 0;
	}


	/*
	 *  Framebuffer update:
	 */

	/*  Border changed?  */
	if (d->border_updated) {
		/*  Fill border with border color:  */
		int rgb = REG(PVRREG_BRDCOLR), addr = 0;
		int x, y, b = rgb & 0xff, g = (rgb >> 8) & 0xff, r = rgb >> 16;
		int skiplen = (d->fb->xsize-2*PVR_MARGIN) * d->fb->bit_depth/8;

		for (y=0; y<d->fb->ysize; y++) {
			int xskip = y < PVR_MARGIN || y >=
			    d->fb->ysize - PVR_MARGIN? -1 : PVR_MARGIN;
			for (x=0; x<d->fb->xsize; x++) {
				if (x == xskip) {
					x = d->fb->xsize - PVR_MARGIN;
					addr += skiplen;
				}
				fb[addr] = r;
				fb[addr+1] = g;
				fb[addr+2] = b;
				addr += 3;
			}
		}

		/*  Full redraw of the framebuffer:  */
		d->fb->update_x1 = 0; d->fb->update_x2 = d->fb->xsize - 1;
		d->fb->update_y1 = 0; d->fb->update_y2 = d->fb->ysize - 1;
	}

	memory_device_dyntrans_access(cpu, cpu->mem, extra, &low, &high);
	if ((int64_t)low != -1)
		pvr_extend_update_region(d, low, high);

	if (d->fb_update_x1 == -1)
		return;

	/*  Copy (part of) the VRAM to the framebuffer:  */
	if (d->fb_update_x2 >= d->xsize)
		d->fb_update_x2 = d->xsize - 1;
	if (d->fb_update_y2 >= d->ysize)
		d->fb_update_y2 = d->ysize - 1;

	vram_ofs += d->fb_update_y1 * bytes_per_line;
	vram_ofs += d->fb_update_x1 * d->bytes_per_pixel;
	pixels_to_copy = (d->fb_update_x2 - d->fb_update_x1 + 1);
	fb_ofs = (d->fb_update_y1 + PVR_MARGIN) * d->fb->bytes_per_line;
	fb_ofs += (d->fb_update_x1 + PVR_MARGIN) * d->fb->bit_depth / 8;

	/*  Copy the actual pixels: (Four manually inlined, for speed.)  */

	switch (d->pixelmode) {
	case 0:	/*  RGB0555 (16-bit)  */
		{
			int y;
			for (y=d->fb_update_y1; y<=d->fb_update_y2; y++) {
				int fo = fb_ofs, vo = vram_ofs;
				for (p=0; p<pixels_to_copy; p++) {
					/*  0rrrrrgg(high) gggbbbbb(low)  */
					fb[fo] = (vram[(vo+1)%VRAM_SIZE] << 1) & 0xf8;
					fb[fo+1] = ((vram[vo%VRAM_SIZE] >> 2) & 0x38) +
					    (vram[(vo+1)%VRAM_SIZE] << 6);
					fb[fo+2] = (vram[vo%VRAM_SIZE] & 0x1f) << 3;
					fo += 3; vo += 2;
				}
				
				vram_ofs += bytes_per_line;
				fb_ofs += d->fb->bytes_per_line;
			}
		}
		break;

	case 1: /*  RGB565 (16-bit)  */
		{
			int y;
			for (y=d->fb_update_y1; y<=d->fb_update_y2; y++) {
				int fo = fb_ofs, vo = vram_ofs;
				for (p=0; p<pixels_to_copy; p++) {
					/*  rrrrrggg(high) gggbbbbb(low)  */
					fb[fo] = vram[(vo+1)%VRAM_SIZE] & 0xf8;
					fb[fo+1] = ((vram[vo%VRAM_SIZE] >> 3) & 0x1c) +
					    (vram[(vo+1)%VRAM_SIZE] << 5);
					fb[fo+2] = (vram[vo%VRAM_SIZE] & 0x1f) << 3;
					fo += 3; vo += 2;
				}
				
				vram_ofs += bytes_per_line;
				fb_ofs += d->fb->bytes_per_line;
			}
		}
		break;

	case 2: /*  RGB888 (24-bit)  */
		{
			int y;
			for (y=d->fb_update_y1; y<=d->fb_update_y2; y++) {
				/*  TODO: Reverse colors, like in the 32-bit case?  */
				memcpy(fb+fb_ofs, vram+(vram_ofs%VRAM_SIZE), 3*pixels_to_copy);
				vram_ofs += bytes_per_line;
				fb_ofs += d->fb->bytes_per_line;
			}
		}
		break;

	case 3: /*  RGB0888 (32-bit)  */
		{
			int y;
			for (y=d->fb_update_y1; y<=d->fb_update_y2; y++) {
				int fo = fb_ofs, vo = vram_ofs;
				for (p=0; p<pixels_to_copy; p++) {
					fb[fo] = vram[(vo+2)%VRAM_SIZE];
					fb[fo+1] = vram[(vo+1)%VRAM_SIZE];
					fb[fo+2] = vram[(vo+0)%VRAM_SIZE];
					fo += 3; vo += 4;
				}
				
				vram_ofs += bytes_per_line;
				fb_ofs += d->fb->bytes_per_line;
			}
		}
		break;
	}

	/*
	 *  Extend the real framebuffer to encompass the area
	 *  just written to:
	 */

	/*  Offset to take the margin into account first...  */
	d->fb_update_x1 += PVR_MARGIN; d->fb_update_y1 += PVR_MARGIN;
	d->fb_update_x2 += PVR_MARGIN; d->fb_update_y2 += PVR_MARGIN;

	if (d->fb_update_x1 < d->fb->update_x1 || d->fb->update_x1 < 0)
		d->fb->update_x1 = d->fb_update_x1;
	if (d->fb_update_x2 > d->fb->update_x2 || d->fb->update_x2 < 0)
		d->fb->update_x2 = d->fb_update_x2;
	if (d->fb_update_y1 < d->fb->update_y1 || d->fb->update_y1 < 0)
		d->fb->update_y1 = d->fb_update_y1;
	if (d->fb_update_y2 > d->fb->update_y2 || d->fb->update_y2 < 0)
		d->fb->update_y2 = d->fb_update_y2;

	/*  Clear the PVR's update region:  */
	d->fb_update_x1 = d->fb_update_x2 =
	    d->fb_update_y1 = d->fb_update_y2 = -1;
}


DEVICE_ACCESS(pvr_vram_alt)
{
	struct pvr_data_alt *d_alt = (struct pvr_data_alt *) extra;
	struct pvr_data *d = d_alt->d;
	size_t i;

	if (writeflag == MEM_READ) {
		/*  Copy from real vram:  */
		for (i=0; i<len; i++) {
			int addr = relative_addr + i;
			addr = ((addr & 4) << 20) | (addr & 3)
			    | ((addr & 0x7ffff8) >> 1);
			data[i] = d->vram[addr % VRAM_SIZE];
		}
		return 1;
	}

	// Writes are only allowed as 16-bit access or higher.
	if (len < sizeof(uint16_t))
		fatal("pvr_vram_alt: write of less than 16 bits attempted?\n");

	/*
	 *  Convert writes to alternative VRAM, into normal writes:
	 */

	for (i=0; i<len; i++) {
		int addr = relative_addr + i;
		addr = ((addr & 4) << 20) | (addr & 3) | ((addr & 0x7ffff8) >> 1);
		// printf("  %08x => alt addr %08x: %02x\n", (int)(relative_addr + i), (int)addr, data[i]);
		d->vram[addr % VRAM_SIZE] = data[i];

		// TODO: This is probably ultra-slow. (Should not be called
		// for every _byte_.)
		pvr_extend_update_region(d, addr, addr);
	}

	return 1;
}


DEVICE_ACCESS(pvr_vram)
{
	struct pvr_data *d = (struct pvr_data *) extra;

	// According to http://mc.pp.se/dc/pvr.html, reads of any size are
	// allowed.
	if (writeflag == MEM_READ) {
		memcpy(data, d->vram + relative_addr, len);
		return 1;
	}

	// However, writes are only allowed as 16-bit access or higher.
	if (len < sizeof(uint16_t))
		fatal("pvr_vram: write of less than 16 bits attempted?\n");

	/*
	 *  Write to VRAM:
	 *
	 *  Calculate which part of the framebuffer this write corresponds to,
	 *  if any, and increase the update region to encompass the written
	 *  memory range.
	 */

	memcpy(d->vram + relative_addr, data, len);
	pvr_extend_update_region(d, relative_addr, relative_addr + len - 1);

	return 1;
}


DEVINIT(pvr)
{
	struct machine *machine = devinit->machine;
	struct pvr_data *d;
	struct pvr_data_alt *d_alt;

	CHECK_ALLOCATION(d = (struct pvr_data *) malloc(sizeof(struct pvr_data)));
	memset(d, 0, sizeof(struct pvr_data));

	CHECK_ALLOCATION(d_alt = (struct pvr_data_alt *) malloc(sizeof(struct pvr_data_alt)));
	memset(d_alt, 0, sizeof(struct pvr_data_alt));

	d_alt->d = d;

	memory_device_register(machine->memory, devinit->name,
	    PVRREG_REGSTART, PVRREG_REGSIZE, dev_pvr_access, d,
	    DM_DEFAULT, NULL);

	/*  8 MB video RAM:  */
	d->vram = (uint8_t *) zeroed_alloc(VRAM_SIZE);
	memory_device_register(machine->memory, "pvr_vram", 0x05000000,
	    VRAM_SIZE, dev_pvr_vram_access, (void *)d,
	    DM_DYNTRANS_OK | DM_DYNTRANS_WRITE_OK
	    | DM_READS_HAVE_NO_SIDE_EFFECTS, d->vram);

	/*  8 MB video RAM, when accessed at 0xa4000000:  */
	memory_device_register(machine->memory, "pvr_alt_vram", 0x04000000,
	    VRAM_SIZE, dev_pvr_vram_alt_access, (void *)d_alt,
	    DM_DEFAULT, NULL);

	/*  Tile Accelerator command area at 0x10000000:  */
	memory_device_register(machine->memory, "pvr_ta",
	    0x10000000, 0x800000, dev_pvr_ta_access, d, DM_DEFAULT, NULL);

	/*  PVR2 DMA registers at 0x5f6800:  */
	memory_device_register(machine->memory, "pvr_dma", 0x005f6800,
	    PVR_DMA_MEMLENGTH, dev_pvr_dma_access, d, DM_DEFAULT, NULL);

	/*  More DMA registers at 0x5f7c00:  */
	memory_device_register(machine->memory, "pvr_dma_more", 0x005f7c00,
	    PVR_DMA_MEMLENGTH, dev_pvr_dma_more_access, d, DM_DEFAULT, NULL);

	d->xsize = 640;
	d->ysize = 480;
	d->pixelmode = 1;	/*  RGB565  */
	d->bytes_per_pixel = 2;

	d->fb = dev_fb_init(machine, machine->memory, INTERNAL_FB_ADDR,
	    VFB_GENERIC, d->xsize + PVR_MARGIN*2, d->ysize + PVR_MARGIN*2,
	    d->xsize + PVR_MARGIN*2, d->ysize + PVR_MARGIN*2,
	    24, "Dreamcast PVR");

	d->vblank_timer = timer_add(PVR_VBLANK_HZ, pvr_vblank_timer_tick, d);

	pvr_reset(d);
	pvr_reset_ta(d);

	machine_add_tickfunction(machine, dev_pvr_fb_tick, d,
	    PVR_FB_TICK_SHIFT);

	return 1;
}

