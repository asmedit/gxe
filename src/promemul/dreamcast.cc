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
 *  COMMENT: Dreamcast PROM emulation
 *
 *  NOTE: This module probably only emulates enough system/BIOS calls to let
 *  NetBSD/dreamcast, Linux/dreamcast, and various KOS demos run. Don't expect
 *  it do be fully functional.
 *
 *  Dreamcast memory layout during startup:
 *
 *  0xa0000000: 2 MB ROM/BIOS. This is copied into RAM (0x8c000000) during
 *              bootup by the real BIOS. In GXemul's fake PROM implementation,
 *              the only thing present at 0xa0000000 is an opcode which
 *              triggers a reboot/shutdown of the emulator.
 *
 *  0x8c000000 - 0x8c0000ff: Various variables and vectors.
 *	  pointer (32-bit word) at 0x8c0000b0: SYSINFO
 *	  pointer (32-bit word) at 0x8c0000b4: ROMFONT
 *	  pointer (32-bit word) at 0x8c0000b8: FLASHROM
 *	  pointer (32-bit word) at 0x8c0000bc: GDROM
 *	  pointer (32-bit word) at 0x8c0000c0: (?) something
 *	  pointer (32-bit word) at 0x8c0000e0: "main menu" call, or "continue
 *					       booting" (?) or something.
 *
 *  0x8c000100: Not on a real Dreamcast, but in GXemul: This area is filled with
 *              a special invalid instruction. Each instruction slot corresponds
 *              to one of the pointers above (SYSINFO, ROMFONT, etc).
 *
 *  0x8c008000: This is where the first 32KB of the data track of a CDROM
 *              gets loaded, also known as the "IP.BIN" part.
 *
 *  0x8c010000: This is where the first binary executable gets loaded. The name
 *              of the executable is given in IP.BIN, and refers to the ISO9660
 *              filename on the CDROM. A common file name is "1ST_READ.BIN".
 *
 *  See http://mc.pp.se/dc/syscalls.html for a description of what the
 *  PROM syscalls do. The symbolic names in this module are the same as on
 *  that page.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include "cpu.h"
#include "machine.h"
#include "memory.h"
#include "misc.h"
#include "thirdparty/dreamcast_pvr.h"

/*  The ROM FONT seems to be located just after 1MB, in a real Dreamcast:  */
#define	DREAMCAST_ROMFONT_BASE		0x80100020
extern unsigned char font8x16[];

/*  Where the machine ID (64-bit) is stored.  */
#define	DREAMCAST_MACHINE_ID_ADDRESS	0x80000068


static int booting_from_cdrom = 0;


/*
 *  dreamcast_romfont_init()
 *
 *  Initialize the ROM font.
 */
static void dreamcast_romfont_init(struct machine *machine)
{
	struct cpu *cpu = machine->cpus[0];
	int i, y, v;
	uint64_t d = DREAMCAST_ROMFONT_BASE;

	/*
	 *  288 narrow glyphs (12 x 24 pixels):
	 *
	 *  Glyphs 1-94 are ASCII characters 33-126, according to
	 *  http://mc.pp.se/dc/syscalls.html#vecB4
	 *
	 *  syscalls.html says "(As there is no glyph for ASCII space, use
	 *  glyph 96 = ISO-8859-1 unbreakable space instead.)", but this does
	 *  not seem to work. Marcus Comstedt's own example (video.s) uses
	 *  char 288 for space instead (but the comment says char 72).
	 *
	 *  TODO: A better looking font. This simply reuses the standard 8x16
	 *  font, which looks odd in Dreamcast programs.
	 */
	for (i=0; i<288; i++) {
		for (y=0; y<24; y+=2) {
			if (y <= 1 || y >= 22)
				v = 0;
			else
				v = random();
			store_byte(cpu, d++, v & 0x3f);
			store_byte(cpu, d++, v & 0xc3);
			store_byte(cpu, d++, v & 0xfc);
		}
	}

	for (i=1; i<=94; i++) {
		d = DREAMCAST_ROMFONT_BASE + i * (24 * 3 / 2);
		int c = 32 + i;
		int u;
		for (y=0; y<24; y+=2) {
			if (y < 4 || y >= 20)
				u = v = 0x00;
			else
				u = font8x16[c*16 + (y-4)], v = font8x16[c*16 + (y-4+1)];

			//  00 00 u7 u6 u5 u4 u3 u2 u1 u0 00 00
			//  00 00 v7 v6 v5 v4 v3 v2 v1 v0 00 00
			//  becomes:
			//  first byte:   00 00 u7 u6 u5 u4 u3 u2
			//  second byte:  u1 u0 00 00 00 00 v7 v6
			//  third byte:   v5 v4 v3 v2 v1 v0 00 00
			store_byte(cpu, d++, u >> 2);
			store_byte(cpu, d++, (u << 6) | (v >> 6));
			store_byte(cpu, d++, v << 2);
		}
	}
	
	// "ISO-8859-1 characters 160-255" (at pos 96..191):
	for (i=96; i<=191; i++) {
		d = DREAMCAST_ROMFONT_BASE + i * (24 * 3 / 2);
		int c = i - 96 + 160;
		int u;
		for (y=0; y<24; y+=2) {
			if (y < 4 || y >= 20)
				u = v = 0;
			else
				u = font8x16[c*16 + (y-4)], v = font8x16[c*16 + (y-4+1)];

			store_byte(cpu, d++, u >> 2);
			store_byte(cpu, d++, (u << 6) | (v >> 6));
			store_byte(cpu, d++, v << 2);
		}
	}

	d = DREAMCAST_ROMFONT_BASE + 289 * (24 * 3 / 2);

	/*  7078 wide glyphs (24 x 24 pixels):  */
	for (i=1; i<7078; i++) {
		for (y=0; y<24; y++) {
			if (y <= 1 || y >= 22)
				v = 0;
			else
				v = 0xff;
			store_byte(cpu, d++, v & 0x3f);
			store_byte(cpu, d++, v);
			store_byte(cpu, d++, v & 0xfc);
		}
	}

	/*  129 VME icons (32 x 32 pixels):  */
	for (i=0; i<129; i++) {
		for (y=0; y<32; y++) {
			if (y <= 1 || y >= 30)
				v = 0;
			else
				v = random();
			store_byte(cpu, d++, v & 0x3f);
			store_byte(cpu, d++, v);
			store_byte(cpu, d++, v);
			store_byte(cpu, d++, v & 0xfc);
		}
	}
}


/*
 *  dreamcast_machine_setup():
 *
 *  Initializes pointers to Dreamcast PROM syscalls.
 */
void dreamcast_machine_setup(struct machine *machine)
{
	int i;
	struct cpu *cpu = machine->cpus[0];

	for (i=0; i<0x50; i+=sizeof(uint32_t)) {
		/*  Store pointer to PROM routine...  */
		store_32bit_word(cpu, 0x8c0000b0 + i, 0x8c000100 + i);

		/*  ... which contains only 1 instruction, a special
		    opcode which triggers PROM emulation:  */
		store_16bit_word(cpu, 0x8c000100 + i, SH_INVALID_INSTR);
	}

	/*  PROM reboot, in case someone jumps to 0xa0000000:  */
	store_16bit_word(cpu, 0xa0000000, SH_INVALID_INSTR);

	/*  Machine ID (64-bit):  */
	store_64bit_word(cpu, DREAMCAST_MACHINE_ID_ADDRESS, 0x0000000000000000ULL);

	dreamcast_romfont_init(machine);

	/*  Return address, if the user program returns: exit.  */
	cpu->cd.sh.pr = 0x8c0000e0 + (0x100 - 0xb0);

	/*  Stack starting at end of RAM.  */
	cpu->cd.sh.r[15] = 0x8c000000 + 16 * 1048576;
}


/*
 *  dreamcast_emul():
 *
 *  We end up here if someone branched or called to 0x8c000100 + ofs,
 *  where ofs is a small number. These addresses correspond to the code reading
 *  a pointer from 0x8c0000b0 + ofs and calling it.
 *
 *  See http://mc.pp.se/dc/syscalls.html for more details.
 */
void dreamcast_emul(struct cpu *cpu)
{
	// cpu->pc is the address where PROM emulation was triggered, but
	// what we are after is the indirect vector that was used to fetch
	// that address.
	int vectorAddr = ((cpu->pc & 0x00ffffff) - 0x100 + 0xb0) | 0x8c000000;

	int r1 = cpu->cd.sh.r[1];
	int r6 = cpu->cd.sh.r[6];
	int r7 = cpu->cd.sh.r[7];

	/*  Special case: Reboot  */
	if ((uint32_t)cpu->pc == 0x80000000 || (uint32_t)cpu->pc == 0xa0000000) {
	 	fatal("[ dreamcast reboot ]\n");
		cpu->running = 0;
		return;
	}

	switch (vectorAddr) {

	case 0x8c0000b0:
		/*  SYSINFO  */
		switch (r7) {
		case 0:	/*  SYSINFO_INIT: Ignored for now.  */
			break;
		case 3:	/*  SYSINFO_ID:  */
			cpu->cd.sh.r[0] = (uint32_t)DREAMCAST_MACHINE_ID_ADDRESS;
			break;
		default:fatal("[ SYSINFO: Unimplemented r7=%i ]\n", r7);
			goto bad;
		}
		break;

	case 0x8c0000b4:
		/*  ROMFONT  */
		switch (r1) {
		case 0:	/*  ROMFONT_ADDRESS  */
			cpu->cd.sh.r[0] = DREAMCAST_ROMFONT_BASE;
			break;
		default:fatal("[ ROMFONT: Unimplemented r1=%i ]\n", r1);
			goto bad;
		}
		break;

	case 0x8c0000b8:
		/*  FLASHROM  */
		switch (r7) {
		case 0:	/*  FLASHROM_INFO  */
			/*  TODO  */
			cpu->cd.sh.r[0] = (uint32_t) -1;
			break;
		case 1:	/*  FLASHROM_READ  */
			/*  TODO  */
			cpu->cd.sh.r[0] = (uint32_t) -1;
			break;
		default:fatal("[ FLASHROM: Unimplemented r7=%i ]\n", r7);
			goto bad;
		}
		break;

	case 0x8c0000bc:
		switch ((int32_t)r6) {
		case 0:	/*  GD-ROM emulation  */
			switch (r7) {
			case 0:	/*  GDROM_SEND_COMMAND  */
				/*  TODO  */
				cpu->cd.sh.r[0] = (uint32_t) -1;
				break;
			case 1:	/*  GDROM_CHECK_COMMAND  */
				/*  TODO  */
				cpu->cd.sh.r[0] = 0;
				break;
			case 2:	/*  GDROM_MAINLOOP  */
				/*  TODO  */
				break;
			case 3:	/*  GDROM_INIT  */
				/*  TODO: Do something here?  */
				break;
			case 4:	/*  GDROM_CHECK_DRIVE  */
				/*  TODO: Return status words  */
				break;
			default:fatal("[ GDROM: Unimplemented r7=%i ]\n", r7);
				goto bad;
			}
			break;
		default:fatal("[ 0xbc: Unimplemented r6=0x%x ]\n", r6);
			goto bad;
		}
		break;

	case 0x8c0000e0:
		/*
		 *  This seems to have two uses:
		 *
		 *  1. KallistiOS calls this from arch_menu(), i.e. to return
		 *     from a running program.
		 *  2. The "licence code" in the IP.BIN code when booting from
		 *     a bootable CD image calls this once the license screen
		 *     has been displayed, and it wants the ROM to jump to
		 *     0x8c00b800 ("Bootstrap 1").
		 *
		 *  The easiest way to support both is probably to keep track
		 *  of whether the IP.BIN code was started by the (software)
		 *  ROM emulation code, or not.
		 */
		if (booting_from_cdrom) {
			debug("[ dreamcast: Switching to bootstrap 1 ]\n");

			booting_from_cdrom = 0;

			// Jump to bootstrap 1
			cpu->pc = 0x8c00b800;
			return;
		} else {
			fatal("[ dreamcast: Returning to main menu. ]\n");
			cpu->running = 0;
		}
		break;

	case 0x8c0000f0:
		/*
		 *  GXemul hack:
		 *
		 *  The vector (word) at 0x8c0000f0 contains the value 0x8c000140.
		 *
		 *  By jumping to this address (0x8c000140), a "boot from
		 *  CDROM" is simulated. Control is transfered to the license
		 *  code in the loaded IP.BIN file.
		 */
		debug("[ dreamcast boot from CDROM ]\n");
		booting_from_cdrom = 1;
		cpu->pc = 0x8c008300;
		return;

	default:goto bad;
	}

	/*  Return from subroutine:  */
	cpu->pc = cpu->cd.sh.pr;

	return;

bad:
	cpu_register_dump(cpu->machine, cpu, 1, 0);
	printf("\n");
	fatal("[ dreamcast_emul(): unimplemented dreamcast PROM call, "
	    "pc=0x%08"PRIx32" (vectorAddr=0x%08"PRIx32") ]\n", (uint32_t)cpu->pc, vectorAddr);
	cpu->running = 0;
	return;
}

