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
 *  COMMENT: SEGA Dreamcast
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sstream>
#include <string>

#include "cpu.h"
#include "device.h"
#include "devices.h"
#include "machine.h"
#include "memory.h"
#include "misc.h"


using namespace std;


MACHINE_SETUP(dreamcast)
{
	machine->machine_name = strdup("Dreamcast");

	if (machine->emulated_hz == 0)
		machine->emulated_hz = 200000000;

	/*  50 MHz SH4 PCLOCK:  */
	machine->cpus[0]->cd.sh.pclock = 50000000;

	if (!machine->x11_md.in_use)
		fprintf(stderr, "-------------------------------------"
		    "------------------------------------------\n"
		    "\n  WARNING!  You are emulating a Dreamcast without -X."
		    "\n            You will miss graphical output!\n\n"
		    "-------------------------------------"
		    "------------------------------------------\n");

	/*
	 *  Physical address layout on the Dreamcast, according to a
	 *  combination of sources:  NetBSD sources, KalistOS sources,
	 *  http://www.boob.co.uk/docs/Dreamcast_memory.txt, and
	 *  http://www.ludd.luth.se/~jlo/dc/memory.txt, and possibly some
	 *  others:
	 *
	 *  0x00000000 - 0x001fffff	Boot ROM (2 MB)
	 *  0x00200000 - 0x0021ffff	Flash (128 KB) (perhaps multiple images afterwards?)
	 *				The bytes read by the Dreamcast PROM during
	 *				boot are:
	 *				    Offset 0x1a000 .. 0x1a004 = 5 bytes debug/startup state?
	 *					hex digits (maybe limited to 0x30, 0x31, 0x32, or 0x33)
	 *				    Offset 0x1a02d .. 0x1a037 = date/time values.
	 *				    Offset 0x1a056 .. 0x1a05d = 8 bytes serial number / machine ID.
	 *  0x005f0000 - ...            ???
	 *  0x005f6800 - ...		PowerVR2 DMA registers
	 *  0x005f6900 - ...		ASIC registers
	 *  0x005f6c00 - ...		Maple registers (controller ports)
	 *  0x005f7000 - ...		GDROM registers
	 *  0x005f7400 - ...		(G2 External DMA registers? Or GDROM?)
	 *  0x005f74e4 - ...		GDROM re-enable disabled drive (?)
	 *  0x005f7800 - ...		G2 External DMA registers
	 *  0x005f7c00 - ...		DMA (?) for some device (PVR related?)
	 *  0x005f8000 - 0x005f9fff	PVR registers (graphics)
	 *  0x00600000 - 0x006007ff	G2 bus: Modem
	 *  0x00600400 - 0x0060047f	G2 bus: LAN Adapter (MB86967) registers?
	 *  0x00620000 - 0x00623fff	G2 bus: Expansion port?
	 *  0x00606900 - ...		???
	 *  0x00700000 - ...		SPU registers (sound)
	 *  0x00702800 - 0x007028ff	???
	 *  0x00702c00 -		Cable select and AICA (?) (*3)
	 *  0x00703xxx - ...		AICA something
	 *  0x00710000 - 0x00710007	RTC registers
	 *  0x00800000 - 0x009fffff	AICA (Sound) RAM (2 MB) (*4)
	 *  0x01000000 - 0x01ffffff	G2 bus or Parallel port registers?
	 *  0x02000000 - ...		CD-ROM port registers
	 *  0x03000000 - 0x03ffffff	G2 bus (?)
	 *  0x04000000 - 0x047fffff	Video RAM (*)     (64-bit)
	 *  0x05000000 - 0x057fffff	Video RAM (8 MB)  (32-bit)
	 *  0x06000000 - 0x067fffff	Video RAM (*)     (64-bit) (copy)
	 *  0x07000000 - 0x077fffff	Video RAM (8 MB)  (32-bit) (copy)
	 *  0x0c000000 - 0x0cffffff	RAM (16 MB)
	 *  0x0e000000 - 0x0effffff	Copy of RAM? (*2)
	 *  0x10000000 - 0x107fffff	Tile Accelerator: command area
	 *  0x10800000 - 0x10ffffff	Tile Accelerator: YUV data
	 *  0x11000000 - 0x11ffffff	Tile Accelerator: Texture data
	 *  0x14000000 - 0x17ffffff	G2 bus (?)
	 *
	 *  (*1) = with banks 0 and 1 switched; 64-bit read/write access...
	 *  (*2) The "luftvarg" 4KB intro uses memory at paddr 0x0ef00000...
	 *  (*3) = See VOUTC in Linux' drivers/video/pvr2fb.c.
	 *  (*4) = It seems that ARM machine code is placed here.
	 */

	dev_ram_init(machine, 0x00000000, 2 * 1024 * 1024,
	    DEV_RAM_RAM /* | DEV_RAM_TRACE_ALL_ACCESSES */, 0x0, "bootrom");

	dev_ram_init(machine, 0x00200000, 128 * 1024,
	    DEV_RAM_RAM /* | DEV_RAM_TRACE_ALL_ACCESSES */, 0x0, "flash");

	dev_ram_init(machine, 0x00600004, 4, DEV_RAM_RAM, 0);
	dev_ram_init(machine, 0x00700000, 0x27ff, DEV_RAM_RAM, 0);
	dev_ram_init(machine, 0x00702800, 256, DEV_RAM_RAM, 0);
	dev_ram_init(machine, 0x00702c00, 4, DEV_RAM_RAM, 0);
	dev_ram_init(machine, 0x00703000, 0x1fff, DEV_RAM_RAM, 0);

	/*  Sound RAM:  */
	dev_ram_init(machine, 0x00800000, 2 * 1048576, DEV_RAM_RAM, 0, "sound_ram");

	/*
	 *  HACK!  TODO: Remove this device at 0x00a00000 once NetBSD has
	 *  been fixed to not clear 6 MB beyound the sound RAM area.
	 */
	dev_ram_init(machine, 0x00a00000, 6 * 1048576, DEV_RAM_RAM, 0, "hack_for_netbsd");

	/*  RAM:  */
	dev_ram_init(machine, 0x0c000000, 16 * 1048576, DEV_RAM_RAM, 0x0);

	/*  Image of RAM:  */
	dev_ram_init(machine, 0x0e000000, 16 * 1048576, DEV_RAM_MIRROR
		| DEV_RAM_MIGHT_POINT_TO_DEVICES, 0x0c000000, "ram_mirror");

	device_add(machine, "pvr");
/*	device_add(machine, "mb8696x addr=0x600400 addr_mult=4");  */
	device_add(machine, "dreamcast_asic");
	device_add(machine, "dreamcast_g2");
	device_add(machine, "dreamcast_gdrom");
	device_add(machine, "dreamcast_maple");
	device_add(machine, "dreamcast_rtc");

	// Add devices as symbols, so that they show up in disassembly/runtime.
	struct memory *mem = cpu->mem;
	for (int i = 0; i < mem->n_mmapped_devices; i++) {
		// Add everything which is not called "ram" (for now).
		if (strcmp(mem->devices[i].name, "ram") == 0) {
			continue;
		}
		
		stringstream ss;
		ss.flags(ios::hex);
		ss << "(" << mem->devices[i].name << "@0x" << mem->devices[i].baseaddr << ")";
		string name = ss.str();		

		add_symbol_name(
		    &machine->symbol_context,
		    mem->devices[i].baseaddr | 0x80000000U,
		    mem->devices[i].length,
		    name.c_str(),
		    0, 0);

		add_symbol_name(
		    &machine->symbol_context,
		    mem->devices[i].baseaddr | 0xa0000000U,
		    mem->devices[i].length,
		    name.c_str(),
		    0, 0);
	}

	if (!machine->prom_emulation)
		return;

	dreamcast_machine_setup(machine);
}


MACHINE_DEFAULT_CPU(dreamcast)
{
	// Hitachi SH4, 200 MHz.  (Or probably an "SH4a".)
	machine->cpu_name = strdup("SH7750");
}


MACHINE_DEFAULT_RAM(dreamcast)
{
	// Note: This is the size of the boot ROM area, since the
	// Dreamcast's RAM isn't located at physical address zero.
	machine->physical_ram_in_mb = 2;
}


MACHINE_REGISTER(dreamcast)
{
	MR_DEFAULT(dreamcast, "Dreamcast", ARCH_SH, MACHINE_DREAMCAST);
	me->set_default_ram = machine_default_ram_dreamcast;
	machine_entry_add_alias(me, "dreamcast");
}

