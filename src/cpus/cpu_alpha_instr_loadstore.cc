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
 *  Alpha load/store instructions.  (Included from cpu_alpha_instr_inc.c.)
 *
 *
 *  Load/store instructions have the following arguments:
 *  
 *  arg[0] = pointer to the register to load to or store from (uint64_t)
 *  arg[1] = pointer to the base register (uint64_t)
 *  arg[2] = offset (as an int32_t)
 *
 *  NOTE:
 *	Alpha byte and word loads (8- and 16-bit) are unsigned, while
 *	32-bit long words are sign-extended up to 64 bits during a load!
 */


#ifndef LS_IGNORE_OFFSET
static void LS_GENERIC_N(struct cpu *cpu, struct alpha_instr_call *ic)
{
#ifdef LS_B
	unsigned char data[1];
#endif
#ifdef LS_W
	unsigned char data[2];
#endif
#ifdef LS_L
	unsigned char data[4];
#endif
#ifdef LS_Q
	unsigned char data[8];
#endif
	uint64_t addr = *((uint64_t *)ic->arg[1]);
	uint64_t data_x;

	addr += (int32_t)ic->arg[2];
#ifdef LS_UNALIGNED
	addr &= ~7;
#endif

#ifdef LS_LOAD
	/*  Load:  */
	if (!cpu->memory_rw(cpu, cpu->mem, addr, data, sizeof(data),
	    MEM_READ, CACHE_DATA)) {
		fatal("store failed: TODO\n");
		exit(1);
	}

	data_x = data[0];
#ifndef LS_B
	data_x += (data[1] << 8);
#ifndef LS_W
	data_x += (data[2] << 16);
	data_x += ((uint64_t)data[3] << 24);
#ifdef LS_L
	data_x = (int64_t)(int32_t)data_x;
#endif
#ifndef LS_L
	data_x += ((uint64_t)data[4] << 32);
	data_x += ((uint64_t)data[5] << 40);
	data_x += ((uint64_t)data[6] << 48);
	data_x += ((uint64_t)data[7] << 56);
#endif
#endif
#endif
	*((uint64_t *)ic->arg[0]) = data_x;
#else
	/*  Store:  */
	data_x = *((uint64_t *)ic->arg[0]);
	data[0] = data_x;
#ifndef LS_B
	data[1] = data_x >> 8;
#ifndef LS_W
	data[2] = data_x >> 16;
	data[3] = data_x >> 24;
#ifndef LS_L
	data[4] = data_x >> 32;
	data[5] = data_x >> 40;
	data[6] = data_x >> 48;
	data[7] = data_x >> 56;
#endif
#endif
#endif

	if (!cpu->memory_rw(cpu, cpu->mem, addr, data, sizeof(data),
	    MEM_WRITE, CACHE_DATA)) {
		fatal("store failed: TODO\n");
		exit(1);
	}

#ifdef LS_LLSC
#ifndef LS_LOAD
	*((uint64_t *)ic->arg[0]) = 1;
#endif
#endif

#endif
}
#endif


static void LS_N(struct cpu *cpu, struct alpha_instr_call *ic)
{
	unsigned char *page;
	uint64_t addr = (*((uint64_t *)ic->arg[1]))
#ifndef LS_IGNORE_OFFSET
	    + (int32_t)ic->arg[2]
#endif
	    ;

	const uint32_t mask1 = (1 << DYNTRANS_L1N) - 1;
	const uint32_t mask2 = (1 << DYNTRANS_L2N) - 1;
	const uint32_t mask3 = (1 << DYNTRANS_L3N) - 1;
	uint32_t x1, x2, x3, c;
	struct DYNTRANS_L2_64_TABLE *l2;
	struct DYNTRANS_L3_64_TABLE *l3;
	x1 = (addr >> (64-DYNTRANS_L1N)) & mask1;
	x2 = (addr >> (64-DYNTRANS_L1N-DYNTRANS_L2N)) & mask2;
	x3 = (addr >> (64-DYNTRANS_L1N-DYNTRANS_L2N-DYNTRANS_L3N)) & mask3;
	/*  fatal("X3: addr=%016"PRIx64" x1=%x x2=%x x3=%x\n",
	    (uint64_t) addr, (int) x1, (int) x2, (int) x3);  */
	l2 = cpu->cd.DYNTRANS_ARCH.l1_64[x1];
	/*  fatal("  l2 = %p\n", l2);  */
	l3 = l2->l3[x2];
	/*  fatal("  l3 = %p\n", l3);  */
#ifdef LS_LOAD
	page = l3->host_load[x3];
#else
	page = l3->host_store[x3];
#endif

#ifdef LS_UNALIGNED
	addr &= ~7;
#endif

#ifdef LS_LLSC
#ifdef LS_LOAD
	/*  TODO: cache-line size!  */
	cpu->cd.alpha.load_linked_addr = addr & ~63;
	cpu->cd.alpha.ll_flag = 1;
#else
	/*  TODO: only invalidate per cache line, not everything!  */
	if (cpu->cd.alpha.ll_flag == 1) {
		int i;
		for (i=0; i<cpu->machine->ncpus; i++)
			cpu->machine->cpus[i]->cd.alpha.ll_flag = 0;
	} else {
		*((uint64_t *)ic->arg[0]) = 0;
		return;
	}
#endif
#endif

	c = addr & 8191;

#ifndef LS_B
	if (c &
#ifdef LS_W
	    1
#endif
#ifdef LS_L
	    3
#endif
#ifdef LS_Q
	    7
#endif
	    ) {
		LS_GENERIC_N(cpu, ic);
		return;
	}
	else
#endif

	if (page != NULL) {
#ifdef LS_LOAD
#ifdef HOST_BIG_ENDIAN
		uint64_t data_x;
		data_x = page[c];
#ifndef LS_B
		data_x += (page[c+1] << 8);
#ifndef LS_W
		data_x += (page[c+2] << 16);
		data_x += ((uint64_t)page[c+3] << 24);
#ifndef LS_L
		data_x += ((uint64_t)page[c+4] << 32);
		data_x += ((uint64_t)page[c+5] << 40);
		data_x += ((uint64_t)page[c+6] << 48);
		data_x += ((uint64_t)page[c+7] << 56);
#endif
#endif
#endif
#ifdef LS_L
		*((uint64_t *)ic->arg[0]) = (int64_t)(int32_t)data_x;
#else
		*((uint64_t *)ic->arg[0]) = data_x;
#endif
#else
#ifdef LS_B
		*((uint64_t *)ic->arg[0]) = page[c];
#endif
#ifdef LS_W
		uint16_t d = *((uint16_t *) (page + c));
		*((uint64_t *)ic->arg[0]) = d;
#endif
#ifdef LS_L
		int32_t d = *((int32_t *) (page + c));
		*((uint64_t *)ic->arg[0]) = (int64_t)d;
#endif
#ifdef LS_Q
		uint64_t d = *((uint64_t *) (page + c));
		*((uint64_t *)ic->arg[0]) = d;
#endif
#endif
#else
		/*  Store:  */
#ifdef HOST_BIG_ENDIAN
		uint64_t data_x = *((uint64_t *)ic->arg[0]);
		page[c] = data_x;
#ifndef LS_B
		page[c+1] = data_x >> 8;
#ifndef LS_W
		page[c+2] = data_x >> 16;
		page[c+3] = data_x >> 24;
#ifndef LS_L
		page[c+4] = data_x >> 32;
		page[c+5] = data_x >> 40;
		page[c+6] = data_x >> 48;
		page[c+7] = data_x >> 56;
#endif
#endif
#endif
#else
		/*  Native byte order:  */
#ifdef LS_B
		page[c] = *((uint64_t *)ic->arg[0]);
#endif
#ifdef LS_W
		uint32_t d = *((uint64_t *)ic->arg[0]);
		*((uint16_t *) (page + c)) = d;
#endif
#ifdef LS_L
		uint32_t d = *((uint64_t *)ic->arg[0]);
		*((uint32_t *) (page + c)) = d;
#endif
#ifdef LS_Q
		uint64_t d = *((uint64_t *)ic->arg[0]);
		*((uint64_t *) (page + c)) = d;
#endif
#endif

#ifdef LS_LLSC
#ifndef LS_LOAD
		*((uint64_t *)ic->arg[0]) = 1;
#endif
#endif

#endif	/*  !LS_LOAD  */
	} else
		LS_GENERIC_N(cpu, ic);
}

