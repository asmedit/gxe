/*
 *  Copyright (C) 2006-2011  Anders Gavare.  All rights reserved.
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
 *  COMMENT: LCA PCI bus, for Alpha machines
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bus_isa.h"
#include "bus_pci.h"
#include "cpu.h"
#include "device.h"
#include "emul.h"
#include "interrupt.h"
#include "machine.h"
#include "memory.h"
#include "misc.h"

#include "thirdparty/alpha_lcareg.h"

#define	LCA_ISA_BASE	(LCA_PCI_SIO + 0x10000000)
#define	LCA_ISA_MEMBASE	(LCA_PCI_SIO + 0x18000000)


struct lca_data {
	struct pci_data		*pci_data;

	uint64_t		ioc_conf;
	uint64_t		tlb_enable;
	uint64_t		window_base_0;
	uint64_t		window_mask_0;
	uint64_t		window_t_base_0;
	uint64_t		window_base_1;
	uint64_t		window_mask_1;
	uint64_t		window_t_base_1;
};


/*
 *  lca_interrupt_assert():
 *
 *  Line 0 = ISA interrupt.
 */
void lca_interrupt_assert(struct interrupt *interrupt)
{
	fatal("lca_interrupt_assert: TODO\n");
	exit(1);
}


/*
 *  lca_interrupt_deassert():
 *
 *  Line 0 = ISA interrupt.
 */
void lca_interrupt_deassert(struct interrupt *interrupt)
{
	fatal("lca_interrupt_deassert: TODO\n");
	exit(1);
}


DEVICE_ACCESS(lca_pci_conf)
{
	uint64_t idata = 0, odata = 0;
	int tag, bus, dev, func, reg;
	struct lca_data *d = (struct lca_data *) extra;

	if (writeflag == MEM_WRITE)
		idata = memory_readmax64(cpu, data, len);

	/*
	 *  1. Decompose the address into a tag.
	 *
	 *  According to NetBSD's lca_pci.c, the address is composed like this:
	 *
	 *	addr = tag << 5 | (regoffset & ~0x03) << 5 | 0x3 << 3
	 */
	reg = (relative_addr >> 5) & 0xfc;
	tag = (relative_addr >> 5) & ~0xff;

	/*
	 *  2. Decompose the tag into bus, dev, and func.
	 *
	 *  The tag can be constructed in one of two ways. On the primary
	 *  bus (nr 0):
	 *
	 *	tag = (1 << (device + 11)) | (function << 8);
	 *
	 *  and on other busses, the tag is a normal:
	 *
	 *	tag = (bus << 16) | (device << 11) | (function << 8)
	 */
	/*  printf("tag = 0x%x\n", (int)tag);  */
	bus = d->ioc_conf & 1;

	if (bus == 0) {
		for (dev=0; dev<21; dev++)
			if (tag & (0x800 << dev))
				break;
		if (dev >= 21) {
			/*  fatal("[ LCA: No bus 0 device? TODO ]\n");
			exit(1);  */
			dev = 0;
		}
	} else {
		fatal("TODO. Non-zero bus.\n");
		exit(1);
	}

	func = (tag >> 8) & 7;
	/*  printf("bus=%i dev=%i func=%i reg=%i\n", bus,dev,func,reg);  */

	/*  Pass PCI accesses onto bus_pci:  */
	bus_pci_setaddr(cpu, d->pci_data, bus, dev, func, reg);
	bus_pci_data_access(cpu, d->pci_data, writeflag == MEM_READ?
	    &odata : &idata, len, writeflag);

	if (writeflag == MEM_READ)
		memory_writemax64(cpu, data, len, odata);

	return 1;
}


DEVICE_ACCESS(lca_isa)
{
	unsigned int ofs, i;
	uint8_t byte;

	relative_addr >>= 5;

	ofs = relative_addr & 3;
	if (ofs > len) {
		fatal("[ ofs=%i len=%i in lca_isa access function. "
		    "aborting ]\n", ofs, len);
		exit(1);
	}

	if (writeflag == MEM_WRITE) {
		byte = data[ofs % len];
		return cpu->memory_rw(cpu, cpu->mem, LCA_ISA_BASE +
		    relative_addr, &byte, 1, writeflag, CACHE_NONE);
	}

	cpu->memory_rw(cpu, cpu->mem, LCA_ISA_BASE + relative_addr,
	    &byte, 1, MEM_READ, CACHE_NONE);

	for (i=0; i<len; i++)
		data[i] = i == ofs? byte : 0x00;

	return 1;
}


DEVICE_ACCESS(lca_ioc)
{
	uint64_t idata = 0, odata = 0;
	struct lca_data *d = (struct lca_data *) extra;

	if (writeflag == MEM_WRITE)
		idata = memory_readmax64(cpu, data, len);

	switch (relative_addr + LCA_IOC_BASE) {

	case LCA_IOC_BASE:
		/*  Ignore? Linux reads from the base at startup.  */
		break;

	case LCA_IOC_CONF:
		if (writeflag == MEM_READ) {
			odata = d->ioc_conf;
		} else {
			d->ioc_conf = idata;
			/*  Only bit 0 is implemented so far, the PCI bus 0 vs
			    bus non-0 selection bit.  */
			if (idata & ~1) {
				fatal("TODO: Write to unimplemented bit of"
				    " IOC_CONF: 0x%x\n", (int)idata);
				exit(1);
			}
		}
		break;

	case LCA_IOC_TBIA:
		/*  TLB Invalidate All.  */
		/*  TODO: For now, let's just ignore it.  */
		break;

	case LCA_IOC_TB_ENA:
		if (writeflag == MEM_READ) {
			odata = d->tlb_enable;
		} else {
			d->tlb_enable = idata;
			/*  TODO: Actually implement this.  */
			if (idata & ~IOC_TB_ENA_TEN) {
				fatal("TODO: LCA_IOC_TB_ENA value "
				    " (0x%"PRIx64") has unimplemented "
				    "bits.\n", (uint64_t)idata);
				exit(1);
			}
		}
		break;

	case LCA_IOC_W_BASE0:
		if (writeflag == MEM_READ) {
			odata = d->window_base_0;
		} else {
			d->window_base_0 = idata;
			/*  TODO: Actually implement this.  */
			if (idata != 0ULL && idata != 0x300800000ULL) {
				fatal("TODO: LCA_IOC_W_BASE0 value differs"
				    " (0x%"PRIx64") from the only implemented"
				    " values\n", (uint64_t)idata);
				exit(1);
			}
		}
		break;

	case LCA_IOC_W_MASK0:
		if (writeflag == MEM_READ) {
			odata = d->window_mask_0;
		} else {
			d->window_mask_0 = idata;
			/*  TODO: Actually implement this.  */
			if (idata != 0x700000ULL) {
				fatal("TODO: LCA_IOC_W_MASK0 value differs"
				    " (0x%"PRIx64") from the only implemented"
				    " value\n", (uint64_t)idata);
				exit(1);
			}
		}
		break;

	case LCA_IOC_W_T_BASE0:
		if (writeflag == MEM_READ) {
			odata = d->window_t_base_0;
		} else {
			d->window_t_base_0 = idata;
			/*  TODO: Actually implement this.  */
		}
		break;

	case LCA_IOC_W_BASE1:
		if (writeflag == MEM_READ) {
			odata = d->window_base_1;
		} else {
			d->window_base_1 = idata;
			/*  TODO: Actually implement this.  */
			if (idata != 0x240000000ULL) {
				fatal("TODO: LCA_IOC_W_BASE1 value differs"
				    " (0x%"PRIx64") from the only implemented"
				    " value\n", (uint64_t)idata);
				exit(1);
			}
		}
		break;

	case LCA_IOC_W_MASK1:
		if (writeflag == MEM_READ) {
			odata = d->window_mask_1;
		} else {
			d->window_mask_1 = idata;
			/*  TODO: Actually implement this.  */
			if (idata != 0x3ff00000ULL) {
				fatal("TODO: LCA_IOC_W_MASK1 value differs"
				    " (0x%"PRIx64") from the only implemented"
				    " value\n", (uint64_t)idata);
				exit(1);
			}
		}
		break;

	case LCA_IOC_W_T_BASE1:
		if (writeflag == MEM_READ) {
			odata = d->window_t_base_1;
		} else {
			d->window_t_base_1 = idata;
			/*  TODO: Actually implement this.  */
		}
		break;

	default:fatal("[ lca_ioc: unimplemented %s to offset 0x%x",
		    writeflag == MEM_WRITE? "write" : "read", (int)
		    relative_addr);
		if (writeflag == MEM_WRITE)
			fatal(": 0x%x", (int)idata);
		fatal(" ]\n");
		exit(1);
	}

	if (writeflag == MEM_READ)
		memory_writemax64(cpu, data, len, odata);

	return 1;
}


DEVINIT(lca)
{
	char *interrupt_path;
	struct interrupt interrupt_template;
	struct lca_data *d;

	CHECK_ALLOCATION(d = (struct lca_data *) malloc(sizeof(struct lca_data)));
	memset(d, 0, sizeof(struct lca_data));

	/*  Register a PCI bus:  */
	d->pci_data = bus_pci_init(
	    devinit->machine,
	    "TODO: irq"		/*  pciirq: TODO  */,
	    LCA_PCI_SIO,	/*  pci device io offset  */
	    0x00000000,		/*  pci device mem offset: TODO  */
	    0x00000000,		/*  PCI portbase: TODO  */
	    0x00000000,		/*  PCI membase: TODO  */
	    "TODO: pci irq base",	/*  PCI irqbase: TODO  */
	    LCA_ISA_BASE,	/*  ISA portbase  */
	    LCA_ISA_MEMBASE,	/*  ISA membase  */
	    "TODO: irqbase isa");                /*  ISA irqbase: TODO  */

	/*  Add the "sio0" controller (as seen by NetBSD):  */
	bus_pci_add(devinit->machine, d->pci_data, devinit->machine->memory,
	    0, 7, 0, "i82378zb");

	memory_device_register(devinit->machine->memory, "lca_pci_conf",
	    LCA_PCI_CONF, 0x20000000, dev_lca_pci_conf_access, (void *)d,
	    DM_DEFAULT, NULL);

	memory_device_register(devinit->machine->memory, "lca_isa",
	    LCA_PCI_SIO, 0x10000 << 5, dev_lca_isa_access, (void *)d,
	    DM_DEFAULT, NULL);

	memory_device_register(devinit->machine->memory, "lca_ioc",
	    LCA_IOC_BASE, 0x20000000, dev_lca_ioc_access, (void *)d,
	    DM_DEFAULT, NULL);

	CHECK_ALLOCATION(interrupt_path = (char *)
	    malloc(strlen(devinit->machine->path) + 10));
	snprintf(interrupt_path, strlen(devinit->machine->path) + 10,
	    "%s.lca", devinit->machine->path);

	memset(&interrupt_template, 0, sizeof(interrupt_template));
	interrupt_template.line = 0;
	interrupt_template.name = interrupt_path;
	interrupt_template.extra = d;
	interrupt_template.interrupt_assert = lca_interrupt_assert;
	interrupt_template.interrupt_deassert = lca_interrupt_deassert;
	interrupt_handler_register(&interrupt_template);

	bus_isa_init(devinit->machine, interrupt_path,
	    BUS_ISA_IDE0 | BUS_ISA_IDE1, LCA_ISA_BASE, LCA_ISA_MEMBASE);

	return 1;
}

