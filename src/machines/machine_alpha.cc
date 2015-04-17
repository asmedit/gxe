/*
 *  Copyright (C) 2005-2011  Anders Gavare.  All rights reserved.
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
 *  COMMENT: DEC Alpha machines
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "cpu.h"
#include "device.h"
#include "devices.h"
#include "machine.h"
#include "memory.h"
#include "misc.h"

#include "thirdparty/alpha_autoconf.h"
#include "thirdparty/alpha_rpb.h"


MACHINE_SETUP(alpha)
{
	struct rpb rpb;
	struct crb crb;
	struct ctb ctb;
	struct mddt mddt;
	struct pcs *pcs = (struct pcs *) malloc(sizeof(struct pcs) * machine->ncpus);
	int i;

	switch (machine->machine_subtype) {

	case ST_ALPHABOOK1:
		machine->machine_name = strdup("AlphaBook 1");
		if (machine->emulated_hz == 0)
			machine->emulated_hz = 233000000;
		device_add(machine, "lca");
		break;

	case ST_DEC_4100:
		machine->machine_name = strdup("AlphaServer 4100");
		break;

	case ST_DEC_3000_300:
		machine->machine_name = strdup("DEC 3000/300");
		machine->main_console_handle = (size_t)device_add(machine,
		    "z8530 addr=0x1b0200000 irq=0 addr_mult=4");
		break;

	case ST_EB164:
		machine->machine_name = strdup("EB164");
		break;

	default:fatal("Unimplemented Alpha machine type %i\n",
		    machine->machine_subtype);
		exit(1);
	}

	if (!machine->prom_emulation)
		return;

	/*  These are used by NetBSD/alpha:  */
	/*  a0 = First free Page Frame Number  */
	/*  a1 = PFN of current Level 1 page table  */
	/*  a2 = Bootinfo magic  */
	/*  a3 = Bootinfo pointer  */
	/*  a4 = Bootinfo version  */
	cpu->cd.alpha.r[ALPHA_A0] = 16*1024*1024 / 8192;
	cpu->cd.alpha.r[ALPHA_A1] = 0;	/*  TODO  */
	cpu->cd.alpha.r[ALPHA_A2] = 0;	/*  Note: NOT ALPHA_BOOTINFO_MAGIC  */
	cpu->cd.alpha.r[ALPHA_A3] = 0;	/*  TODO  */
	cpu->cd.alpha.r[ALPHA_A4] = 1;

	/*
	 *  HWRPB: Hardware Restart Parameter Block
	 *
	 *  TODO: Almost everything.
	 */
	memset(&rpb, 0, sizeof(struct rpb));
	store_64bit_word_in_host(cpu, (unsigned char *)
	    &(rpb.rpb_phys), 0x14000);
	strlcpy((char *)&(rpb.rpb_magic), "HWRPB", 8);
	store_64bit_word_in_host(cpu, (unsigned char *)
	    &(rpb.rpb_version), HWRPB_DSRDB_MINVERS);
	store_64bit_word_in_host(cpu, (unsigned char *)
	    &(rpb.rpb_size), sizeof(struct rpb));
	store_64bit_word_in_host(cpu, (unsigned char *)
	    &(rpb.rpb_page_size), 8192);
	strlcpy((char *)&(rpb.rpb_ssn), "123456789", 10);
	store_64bit_word_in_host(cpu, (unsigned char *)
	    &(rpb.rpb_type), machine->machine_subtype);
	store_64bit_word_in_host(cpu, (unsigned char *)
	    &(rpb.rpb_cc_freq), machine->emulated_hz);
	store_64bit_word_in_host(cpu, (unsigned char *)
	    &(rpb.rpb_intr_freq), 1024 << 12);
	store_64bit_word_in_host(cpu, (unsigned char *)
	    &(rpb.rpb_pcs_cnt), machine->ncpus);
	store_64bit_word_in_host(cpu, (unsigned char *)
	    &(rpb.rpb_pcs_size), sizeof(struct pcs));
	store_64bit_word_in_host(cpu, (unsigned char *)
	    &(rpb.rpb_pcs_off), PCS_ADDR - HWRPB_ADDR);
	store_64bit_word_in_host(cpu, (unsigned char *)
	    &(rpb.rpb_ctb_off), CTB_ADDR - HWRPB_ADDR);
	store_64bit_word_in_host(cpu, (unsigned char *)
	    &(rpb.rpb_crb_off), CRB_ADDR - HWRPB_ADDR);
	store_64bit_word_in_host(cpu, (unsigned char *)
	    &(rpb.rpb_memdat_off), MEMDAT_ADDR - HWRPB_ADDR);

	/*  CTB: Console Terminal Block  */
	memset(&ctb, 0, sizeof(struct ctb));
	store_64bit_word_in_host(cpu, (unsigned char *)
	    &(ctb.ctb_term_type), machine->x11_md.in_use?
	    CTB_GRAPHICS : CTB_PRINTERPORT);

	/*  CRB: Console Routine Block  */
	memset(&crb, 0, sizeof(struct crb));
	store_64bit_word_in_host(cpu, (unsigned char *)
	    &(crb.crb_v_dispatch), CRB_ADDR - 0x100);
	store_64bit_word(cpu, CRB_ADDR - 0x100 + 8, PROM_ENTRY_PADDR);
	store_64bit_word_in_host(cpu, (unsigned char *)
	    &(crb.crb_v_fixup), CRB_ADDR - 0x80);
	store_64bit_word(cpu, CRB_ADDR - 0x80 + 8, PROM_ENTRY_PADDR + 0x800);

	/*  PCS: Processor ID etc.  */
	for (i=0; i<machine->ncpus; i++) {
		memset(&pcs[i], 0, sizeof(struct pcs));
		store_64bit_word_in_host(cpu, (unsigned char *)
		    &(pcs[i].pcs_flags), PCS_RC | PCS_PA | PCS_PP |
		    PCS_CV | PCS_PV | PCS_PMV | PCS_PL);
		store_64bit_word_in_host(cpu, (unsigned char *)
		    &(pcs[i].pcs_proc_type),
		    machine->cpus[i]->cd.alpha.cpu_type.pcs_type);
	}

	/*
	 *  MDDT: Memory Data Descriptor Table. For now, it is a simple
	 *  two-entry table with half of the available RAM in each entry.
	 *  (The values are in number of 8K pages.)
	 *  The first 16 MB are not included (the kernel lives there).
	 *  The last 1 MB is not included either, it is reserved for bootup
	 *  and similar.
	 */
	memset(&mddt, 0, sizeof(struct mddt));
	memset(&mddt.mddt_clusters[0], 0, sizeof(struct mddt_cluster));
	memset(&mddt.mddt_clusters[1], 0, sizeof(struct mddt_cluster));
	store_64bit_word_in_host(cpu, (unsigned char *)
	    &(mddt.mddt_cluster_cnt), 2);
	store_64bit_word_in_host(cpu, (unsigned char *)
	    &(mddt.mddt_clusters[0].mddt_pfn), 16 * 128);
	store_64bit_word_in_host(cpu, (unsigned char *)
	    &(mddt.mddt_clusters[0].mddt_pg_cnt),
	    (machine->physical_ram_in_mb/2 - 16) * 128);
	store_64bit_word_in_host(cpu, (unsigned char *)
	    &(mddt.mddt_clusters[1].mddt_pfn),
	    machine->physical_ram_in_mb/2 * 128);
	store_64bit_word_in_host(cpu, (unsigned char *)
	    &(mddt.mddt_clusters[1].mddt_pg_cnt),
	    (machine->physical_ram_in_mb/2) * 128);

	/*
	 *  Place a special "hack" palcode call at PROM_ENTRY_PADDR and
	 *  PROM_ENTRY_PADDR + 0x800:
	 *  (Hopefully nothing else will be there.)
	 */
	store_32bit_word(cpu, PROM_ENTRY_PADDR, 0x3fffffe);
	store_32bit_word(cpu, PROM_ENTRY_PADDR + 0x800, 0x3fffffd);

	store_buf(cpu, HWRPB_ADDR, (char *)&rpb, sizeof(struct rpb));
	store_buf(cpu, CTB_ADDR, (char *)&ctb, sizeof(struct ctb));
	store_buf(cpu, CRB_ADDR, (char *)&crb, sizeof(struct crb));
	store_buf(cpu, MEMDAT_ADDR, (char *)&mddt, sizeof(struct mddt));
	store_buf(cpu, PCS_ADDR, (char *)pcs, sizeof(struct pcs) *
	    machine->ncpus);

	free(pcs);
}


MACHINE_DEFAULT_CPU(alpha)
{
	switch (machine->machine_subtype) {

	case ST_ALPHABOOK1:
		machine->cpu_name = strdup("21066");
		break;

	case ST_DEC_4100:
		machine->cpu_name = strdup("21164A-2");
		break;

	case ST_DEC_3000_300:
		machine->cpu_name = strdup("21064");
		break;

	case ST_EB164:
		machine->cpu_name = strdup("21164PC");
		break;

	default:fatal("Unimplemented Alpha machine type %i\n",
		    machine->machine_subtype);
		exit(1);
	}
}


MACHINE_DEFAULT_RAM(alpha)
{
	machine->physical_ram_in_mb = 128;
}


MACHINE_REGISTER(alpha)
{
	MR_DEFAULT(alpha, "Alpha", ARCH_ALPHA, MACHINE_ALPHA);

	machine_entry_add_alias(me, "alpha");

	machine_entry_add_subtype(me, "AlphaBook 1", ST_ALPHABOOK1,
	    "alphabook1", NULL);

	machine_entry_add_subtype(me, "AlphaServer 4100", ST_DEC_4100,
	    "alphaserver4100", NULL);

	machine_entry_add_subtype(me, "DEC 3000/300", ST_DEC_3000_300,
	    "3000/300", NULL);

	machine_entry_add_subtype(me, "EB164", ST_EB164,
	    "eb164", NULL);

	me->set_default_ram = machine_default_ram_alpha;
}

