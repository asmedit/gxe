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
 *  COMMENT: Dreamcast GD-ROM
 *
 *  TODO: This is mostly just a dummy so far. It is enough for NetBSD/dreamcast
 *  to read the GD-ROM (or actually, just a particular Live CD). It shouldn't
 *  be assumed to work for anything else.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cpu.h"
#include "device.h"
#include "diskimage.h"
#include "machine.h"
#include "memory.h"
#include "misc.h"

#include "thirdparty/dreamcast_sysasicvar.h"


// #define debug fatal

#define	NREGS_GDROM_DMA		(0x100/sizeof(uint32_t))

struct dreamcast_gdrom_data {
	// 0x005f7000: GDROM control registers
	uint8_t		busy;		/*  Busy status  */
	uint8_t		stat;		/*  Status  */
	int		cnt;		/*  Data length in bytes  */
	uint8_t		cond;

	int		cmd_count;
	uint8_t		cmd[12];

	uint8_t		*data;
	int		data_len;
	int		cur_data_offset;
	int		cur_cnt;

	// 0x005f7400: GDROM DMA
	uint32_t	dma_reg[NREGS_GDROM_DMA];
};

/*  Register offsets, from NetBSD:  */
#define	GDROM_BUSY		0x18
#define	GDROM_DATA		0x80
#define	GDROM_REGX		0x84
#define	GDROM_UNKNOWN_0x88	0x88
#define	GDROM_STAT		0x8c
#define	GDROM_CNTLO		0x90
#define	GDROM_CNTHI		0x94
#define	GDROM_COND		0x9c

// See the interrupt routine (gdrom_intr()) in NetBSD's dreamcast/dev/gdrom.c:
#define	COND_DATA_AVAIL		0x08


static void alloc_data(struct dreamcast_gdrom_data *d)
{
	d->data_len = d->cnt;

	CHECK_ALLOCATION(d->data = (uint8_t *) malloc(d->data_len));
	memset(d->data, 0, d->data_len);
}


static void handle_command(struct cpu *cpu, struct dreamcast_gdrom_data *d)
{
	int64_t sector_nr, sector_count;
	int i, res;

	debug("[ GDROM cmd: ");
	for (i=0; i<12; i++)
		debug("%02x ", d->cmd[i]);
	debug("(cnt=%i) ]\n", d->cnt);

	if (d->data != NULL)
		free(d->data);
	d->data = NULL;
	d->cur_data_offset = 0;
	d->cur_cnt = 0;

	switch (d->cmd[0]) {

	case 0x14:
		/*
		 *  Read Table-Of-Contents:
		 *
		 *  See NetBSD's sys/arch/dreamcast/dev/gdrom.c: gdrom_read_toc().
		 */
		if (d->cnt != 408) {
			fatal("GDROM Read TOC not 408 bytes?\n");
			exit(1);
		}
		alloc_data(d);

		/*
		 *  From David Brownlee:
		 *  "TOC from test booted real CD image on NetBSD/dreamcast
		 *  01000096,41002e4c,ffffffff * 97,01010000,41020000,6100e641,"
		 *
		 *  TODO: Perhaps generalize this in the future, to support
		 *  disk images with multiple TOCs and/or tracks.
		 */
		memset(d->data, 0xff, d->cnt); /* Default data to 0xff */

		d->data[0*4]   = 0x10;  /* Track 1 */
		d->data[0*4+1] = 0;
		d->data[0*4+2] = 0;
		d->data[0*4+3] = 0x96;

		d->data[1*4]   = 0x41;  /* Track 2 */
		d->data[1*4+1] = 0;
		d->data[1*4+2] = 0x2e;
		d->data[1*4+3] = 0x4c;

		d->data[99*4]   = 0x01; /*  First track  */
		d->data[99*4+1] = 0x01;
		d->data[99*4+2] = 0;
		d->data[99*4+3] = 0;

		d->data[100*4]   = 0x41; /*  Last track  */
		d->data[100*4+1] = 0x02;
		d->data[100*4+2] = 0;
		d->data[100*4+3] = 0;

		d->data[101*4]   = 0x61; /*  Leadout  */
		d->data[101*4+1] = 0;
		d->data[101*4+2] = 0xe6;
		d->data[101*4+3] = 0x41;
 
		break;

	case 0x30:
		/*
		 *  Read sectors:
		 *
		 *  See NetBSD's sys/arch/dreamcast/dev/gdrom.c: gdrom_read_sectors().
		 */
		if (d->cmd[1] == 0x24) {
			fatal("GDROM unimplemented data format 0x%02x. Continuing anway.\n", d->cmd[1]);
		} else if (d->cmd[1] != 0x20) {
			fatal("GDROM unimplemented data format 0x%02x\n",
			    d->cmd[1]);
			exit(1);
		}
		sector_nr = d->cmd[2] * 65536 + d->cmd[3] * 256 + d->cmd[4];
		sector_count = d->cmd[8] * 65536 + d->cmd[9] * 256 + d->cmd[10];

		// NetBSD/dreamcast uses correct length, but the Dreamcast PROM
		// uses len = 0 (!), but sector count = 7,
		// which means len = 0x3800. It also sets up DMA to
		// transfer 0x3800 bytes, so I'm assuming that the
		// sector count is to be trusted more than the length.
		if (d->cnt == 0)
			d->cnt = 2048 * sector_count;

		if (sector_count * 2048 != d->cnt) {
			fatal("Huh? GDROM data_len=0x%x, but sector_count"
			    "=0x%x\n", (int)d->cnt, (int)sector_count);
			exit(1);
		}

		alloc_data(d);

		// Hm. This is an ugly hack to make a NetBSD/dreamcast
		// live-cd work. It should be fixed (i.e. removed).
		// When running with -Q (i.e. no PROM software emulation),
		// experiments with the Dreamcast PROM can be made instead.
		if (cpu->machine->prom_emulation) {
			// printf("sector nr step 1 = %i\n", (int)sector_nr);

			// Hack to get NetBSD/dreamcast to work:
			// See the variable "openpart_start" in NetBSD's
			// sys/arch/dreamcast/dev/gdrom.c:
			sector_nr -= 150;

			// printf("sector nr step 2 = %i\n", (int)sector_nr);
		} else {
			// printf("sector nr step 1 = %i\n", (int)sector_nr);
			sector_nr -= 45150;
			// printf("sector nr step 2 = %i\n", (int)sector_nr);
			sector_nr += (diskimage_get_baseoffset(cpu->machine, 0, DISKIMAGE_IDE) / 2048);
			// printf("sector nr step 3 = %i\n", (int)sector_nr);
		}

		res = diskimage_access(cpu->machine, 0, DISKIMAGE_IDE,
		    0, sector_nr * 2048, d->data, d->data_len);
		if (!res) {
			fatal("GDROM: diskimage_access failed? TODO\n");
			free(d->data);
			d->data = NULL;
		}

		/* {
			printf("(Dump of GDROM sector %i: \"", (int)sector_nr);
			for (int k = 0; k < 256; ++k)
				printf("%c", d->data[k] >= 32 ? d->data[k] : '.');
			printf("\")\n");
		} */

		break;

	case 0x70:
		/*
		 *  Mount:
		 *
		 *  See NetBSD's sys/arch/dreamcast/dev/gdrom.c: gdrom_mount_disk().
		 */

		// Note/TODO: This is ignored for now.
		break;

	default:fatal("GDROM handle_command: unimplemented command 0x%02x"
		    "\n", d->cmd[0]);
		exit(1);
	}

	// Any resulting data? Then set COND_DATA_AVAIL. Otherwise, clear
	// that bit, and set count to zero.
	if (d->data != NULL) {
		d->cond |= COND_DATA_AVAIL;
	} else {
		d->cnt = 0;
		d->cond &= ~COND_DATA_AVAIL;
	}

	// NetBSD seems to sometimes request 32 sectors (2048 bytes each), i.e.
	// 65536 bytes. That is represented as count = 0x0000 (when NetBSD reads
	// from the CNTHI and CNTLO registers). That does not work very well.
	// This is a hack/workaround for that.
	if (d->cnt == 65536)
		d->cnt = 32768;

	SYSASIC_TRIGGER_EVENT(SYSASIC_EVENT_GDROM);
}

void dreamcast_gdrom_update_stat(struct cpu *cpu, struct dreamcast_gdrom_data *d)
{
	// See NetBSD's gdrom.c.
	d->stat = 6;

	if (diskimage_exist(cpu->machine, 0, DISKIMAGE_IDE)) {
		d->stat = 0;
	}
}


DEVICE_ACCESS(dreamcast_gdrom)
{
	struct dreamcast_gdrom_data *d = (struct dreamcast_gdrom_data *) extra;
	uint64_t idata = 0, odata = 0;

	if (writeflag == MEM_WRITE)
		idata = memory_readmax64(cpu, data, len);

	switch (relative_addr) {

	case GDROM_BUSY:
		if (writeflag == MEM_READ) {
			odata = d->busy;
		} else {
			// fatal("[ Write to GDROM_BUSY: 0x%08x ]\n", (int)idata);
			// I'm assuming that writing bits to BUSY clears them.
			d->busy &= ~idata;
		}
		break;

	case GDROM_DATA:
		if (len != sizeof(uint16_t)) {
			fatal("Non-16bit GDROM data access? TODO\n");
			exit(1);
		}

		if (writeflag == MEM_READ) {
			if (!(d->cond & COND_DATA_AVAIL)) {
				fatal("Read from GDROM_DATA when no data"
				    " is available? TODO\n");
				exit(1);
			}

			if (d->cur_data_offset < d->data_len) {
				odata = d->data[d->cur_data_offset ++];
				odata |= (d->data[d->cur_data_offset ++] << 8);
				d->cur_cnt += sizeof(uint16_t);
				if (d->cur_cnt >= d->cnt) {
					if (d->cur_data_offset >= d->data_len) {
						d->cond &= ~COND_DATA_AVAIL;
					} else {
						d->cnt = d->data_len - d->cur_data_offset;
						d->cur_cnt = 0;
					}

					SYSASIC_TRIGGER_EVENT(SYSASIC_EVENT_GDROM);
				}
			} else {
				fatal("Read too much from GDROM_DATA\n");
				exit(1);
			}
		} else {
			if (d->busy & 0x08) {
				if (d->cmd_count >= 12) {
					fatal("Too much GDROM_DATA?\n");
					exit(1);
				}
				/*  Add data to cmd:  */
				d->cmd[d->cmd_count++] = idata;
				d->cmd[d->cmd_count++] = idata >> 8;
				if (d->cmd_count == 12) {
					d->busy &= ~0x08;
					handle_command(cpu, d);
				}
			} else {
				fatal("Write to GDROM_DATA, but not waiting"
				    " for data?\n");
				exit(1);
			}
		}
		break;

	case GDROM_REGX:
		if (writeflag == MEM_READ) {
			debug("[ Read to GDROM_REGX? ]\n");
		} else {
			/*  NetBSD/dreamcast writes 0 here.  */
			if (idata != 0)
				debug("[ Write 0x%x to GDROM_REGX? ]\n", (int)idata);
		}
		break;

	case GDROM_UNKNOWN_0x88:
		if (writeflag == MEM_READ) {
			fatal("Read from GDROM_UNKNOWN_0x88?\n");
			// exit(1);
		} else {
			if (idata != 0xb) {
				fatal("[ Write to GDROM_UNKNOWN_0x88: TODO ]\n");
				exit(1);
			}
		}
		break;

	case GDROM_STAT:
		if (writeflag == MEM_READ) {
			dreamcast_gdrom_update_stat(cpu, d);
			odata = d->stat;
		} else {
			fatal("[ Write to GDROM_STAT? ]\n");
/*			exit(1);  */
		}
		break;

	case GDROM_CNTLO:
		if (writeflag == MEM_READ) {
			odata = d->cnt & 0xff;
		} else {
			d->cnt = (d->cnt & 0xff00) | (idata & 0xff);
		}
		break;

	case GDROM_CNTHI:
		if (writeflag == MEM_READ) {
			odata = (d->cnt >> 8) & 0xff;
		} else {
			d->cnt = (d->cnt & 0x00ff) | ((idata & 0xff) << 8);
		}
		break;

	case GDROM_COND:
		if (writeflag == MEM_READ) {
			odata = d->cond;
		} else {
			d->cond = idata;

			/*
			 *  NetBSD/dreamcast writes 0xa0 to GDROM_COND to
			 *  start a command. See gdrom_do_command() in NetBSD's
			 *  sys/arch/dreamcast/dev/gdrom.c.
			 *
			 *  Before sending anything, NetBSD expects:
			 *   o) the lowest 4 bits of STAT to not be 6.
			 *
			 *  After writing 0xa0 to GDROM_COND, NetBSD expects:
			 *   o) (BUSY & 0x88) to be 0x08.
			 *
			 *  NetBSD then sends 6 16-bit data words to GDROM_DATA.
			 */
			if (idata == 0xa0) {
				d->stat = 0;	/*  TODO  */
				d->busy |= 0x08;
				d->cmd_count = 0;
			} else if (idata == 0xef) {
				debug("dreamcast_gdrom: ROM: TODO\n");
				SYSASIC_TRIGGER_EVENT(SYSASIC_EVENT_GDROM);
			} else {
				fatal("dreamcast_gdrom: unimplemented "
				    "GDROM_COND = 0x%02x\n", (int)idata);
				exit(1);
			}
		}
		break;

	default:if (writeflag == MEM_READ) {
			fatal("[ dreamcast_gdrom: read from addr 0x%x ]\n",
			    (int)relative_addr);
		} else {
			fatal("[ dreamcast_gdrom: write to addr 0x%x: 0x%x ]\n",
			    (int)relative_addr, (int)idata);
		}

		exit(1);
	}

	if (writeflag == MEM_READ)
		memory_writemax64(cpu, data, len, odata);

	return 1;
}


DEVICE_ACCESS(dreamcast_gdrom_dma)
{
	struct dreamcast_gdrom_data *d = (struct dreamcast_gdrom_data *) extra;
	uint64_t idata = 0, odata = 0;

	if (writeflag == MEM_WRITE)
		idata = memory_readmax64(cpu, data, len);

	/*  Default read:  */
	if (writeflag == MEM_READ)
		odata = d->dma_reg[relative_addr / sizeof(uint32_t)];

	switch (relative_addr) {

	case 0x04:	// destination address? e.g. 0x8c008000
	case 0x08:	// DMA length in bytes? e.g. 0x3800
	case 0x0c:	// count? e.g. 1
	case 0x14:	// "enable"?
		break;

	case 0x18:
		// GDROM DMA start?
		if (idata != 0) {
			if (d->dma_reg[0x0c / sizeof(uint32_t)] == 1 &&
			    d->dma_reg[0x14 / sizeof(uint32_t)] == 1) {
				// GDROM DMA transfer.
				uint32_t dst = d->dma_reg[0x04 / sizeof(uint32_t)];
				int length = d->dma_reg[0x08 / sizeof(uint32_t)];
				fatal("[ dreamcast_gdrom_dma: Transfering %i bytes to 0x%08"PRIx32" ]\n", length, dst);

				if (d->data == NULL) {
					fatal("dreamcast_gdrom_dma: DMA transfer but d->data is NULL. TODO\n");
					exit(1);
				}

				dst &= 0x0fffffff;	// 0x8c008000 => 0x0c008000
				cpu->memory_rw(cpu, cpu->mem, dst,
				    d->data, d->data_len, MEM_WRITE, PHYSICAL);

				SYSASIC_TRIGGER_EVENT(SYSASIC_EVENT_GDROM_DMA);

				idata = 0;
			} else {
				fatal("Unimplemented GDROM DMA start? TODO\n");
				fatal("  %08x\n", (int) d->dma_reg[4 / sizeof(uint32_t)]);
				fatal("  %08x\n", (int) d->dma_reg[8 / sizeof(uint32_t)]);
				fatal("  %08x\n", (int) d->dma_reg[0xc / sizeof(uint32_t)]);
				fatal("  %08x\n", (int) d->dma_reg[0x14 / sizeof(uint32_t)]);
				exit(1);
			}
		}
		break;			

	default:if (writeflag == MEM_READ) {
			fatal("[ dreamcast_gdrom_dma: read from addr 0x%x ]\n",
			    (int)relative_addr);
		} else {
			fatal("[ dreamcast_gdrom_dma: write to addr 0x%x: "
			    "0x%x ]\n", (int)relative_addr, (int)idata);
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


DEVINIT(dreamcast_gdrom)
{
	struct dreamcast_gdrom_data *d;

	CHECK_ALLOCATION(d = (struct dreamcast_gdrom_data *) malloc(sizeof(struct dreamcast_gdrom_data)));
	memset(d, 0, sizeof(struct dreamcast_gdrom_data));

	memory_device_register(devinit->machine->memory, devinit->name,
	    0x005f7000, 0x100, dev_dreamcast_gdrom_access, d,
	    DM_DEFAULT, NULL);

	memory_device_register(devinit->machine->memory, "gdrom_dma", 0x005f7400,
	    0x80, dev_dreamcast_gdrom_dma_access, d, DM_DEFAULT, NULL);

	return 1;
}

