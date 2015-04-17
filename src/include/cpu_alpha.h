#ifndef	CPU_ALPHA_H
#define	CPU_ALPHA_H

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
 *  Alpha CPU definitions.
 */

#include "misc.h"

#include "thirdparty/alpha_cpu.h"


/*  ALPHA CPU types:  */
struct alpha_cpu_type_def { 
	const char	*name;
	uint64_t	pcs_type;	/*  See alpha_rpb.h  */
	int		features;
	int		implver;
	int		icache_shift;
	int		ilinesize;
	int		iway;
	int		dcache_shift;
	int		dlinesize;
	int		dway;
	int		l2cache_shift;
	int		l2linesize;
	int		l2way;
};

/*  TODO: More features  */
#define	ALPHA_FEATURE_BWX		1

#define ALPHA_CPU_TYPE_DEFS	{					\
	{ "21064",	0x000000002ULL, 0, 0, 16,5,2, 16,5,2,  0,0,0 },	\
	{ "21066",	0x200000004ULL, 0, 0, 16,5,2, 16,5,2,  0,0,0 },	\
	{ "21164",	0x000000005ULL, 0, 1, 16,5,2, 16,5,2,  0,0,0 },	\
	{ "21164A-2",	0x000000007ULL, 0, 1, 16,5,2, 16,5,2,  0,0,0 },	\
	{ "21164PC",	0x000000009ULL, 0, 1, 16,5,2, 16,5,2,  0,0,0 },	\
	{ "21264",	0x00000000dULL, 0, 2, 16,5,2, 16,5,2,  0,0,0 },	\
	{ "21364",	0x000000000ULL, 0, 3, 16,5,2, 16,5,2,  0,0,0 },	\
	{ NULL,		0x000000000ULL, 0, 0,  0,0,0,  0,0,0,  0,0,0 }	}


struct cpu_family;

/*  ALPHA_KENTRY_INT .. ALPHA_KENTRY_SYS  */
#define	N_ALPHA_KENTRY		6

#define	ALPHA_V0		0
#define	ALPHA_T0		1
#define	ALPHA_T1		2
#define	ALPHA_T2		3
#define	ALPHA_T3		4
#define	ALPHA_T4		5
#define	ALPHA_T5		6
#define	ALPHA_T6		7

#define	ALPHA_T7		8
#define	ALPHA_S0		9
#define	ALPHA_S1		10
#define	ALPHA_S2		11
#define	ALPHA_S3		12
#define	ALPHA_S4		13
#define	ALPHA_S5		14
#define	ALPHA_FP		15

#define	ALPHA_A0		16
#define	ALPHA_A1		17
#define	ALPHA_A2		18
#define	ALPHA_A3		19
#define	ALPHA_A4		20
#define	ALPHA_A5		21
#define	ALPHA_T8		22
#define	ALPHA_T9		23

#define	ALPHA_T10		24
#define	ALPHA_T11		25
#define	ALPHA_RA		26
#define	ALPHA_T12		27
#define	ALPHA_AT		28
#define	ALPHA_GP		29
#define	ALPHA_SP		30
#define	ALPHA_ZERO		31

#define	N_ALPHA_REGS		32

#define ALPHA_REG_NAMES		{				\
	"v0", "t0", "t1", "t2", "t3", "t4", "t5", "t6",		\
	"t7", "s0", "s1", "s2", "s3", "s4", "s5", "fp",		\
	"a0", "a1", "a2", "a3", "a4", "a5", "t8", "t9",		\
	"t10", "t11", "ra", "t12", "at", "gp", "sp", "zero" 	}


/*  Dyntrans definitions:  */

#define	ALPHA_N_IC_ARGS			3
#define	ALPHA_INSTR_ALIGNMENT_SHIFT	2
#define	ALPHA_IC_ENTRIES_SHIFT		11
#define	ALPHA_IC_ENTRIES_PER_PAGE	(1 << ALPHA_IC_ENTRIES_SHIFT)
#define	ALPHA_PC_TO_IC_ENTRY(a)		(((a)>>ALPHA_INSTR_ALIGNMENT_SHIFT) \
					& (ALPHA_IC_ENTRIES_PER_PAGE-1))
#define	ALPHA_ADDR_TO_PAGENR(a)		((a) >> (ALPHA_IC_ENTRIES_SHIFT \
					+ ALPHA_INSTR_ALIGNMENT_SHIFT))

#define	ALPHA_MAX_VPH_TLB_ENTRIES	128

#define	ALPHA_L2N		17
#define	ALPHA_L3N		17

DYNTRANS_MISC_DECLARATIONS(alpha,ALPHA,uint64_t)
DYNTRANS_MISC64_DECLARATIONS(alpha,ALPHA,uint8_t)


#define	ALPHA_PAGESHIFT		13


struct alpha_cpu {
	struct alpha_cpu_type_def	cpu_type;


	/*
	 *  General Purpose Registers:
	 */

	uint64_t		r[N_ALPHA_REGS];	/*  Integer  */
	uint64_t		f[N_ALPHA_REGS];	/*  Floating Point  */

	uint64_t		fpcr;			/*  FP Control Reg.  */

	/*  Misc.:  */
	uint64_t		pcc;			/*  Cycle Counter  */
	uint64_t		load_linked_addr;
	int			ll_flag;

	int			irq_asserted;

	/*  OSF1 PALcode specific:  */
	uint64_t		ps;		/*  Processor Status  */
	uint64_t		vptptr;		/*  Virtual Page Table Ptr  */
	uint64_t		sysvalue;
	uint64_t		mces;		/*  Machine Check Error Summary  */
	uint64_t		kgp;		/*  Kernel GP  */
	uint64_t		kentry[N_ALPHA_KENTRY];
	uint64_t		ctx;		/*  Ptr to current PCB (?)  */
	struct alpha_pcb	pcb;		/*  Process Control Block  */


	/*
	 *  Instruction translation cache and Virtual->Physical->Host
	 *  address translation:
	 */
	DYNTRANS_ITC(alpha)
	VPH_TLBS(alpha,ALPHA)
	VPH64(alpha,ALPHA)
};


/*  cpu_alpha.c:  */
void alpha_update_translation_table(struct cpu *cpu, uint64_t vaddr_page,
	unsigned char *host_page, int writeflag, uint64_t paddr_page);
void alpha_invalidate_translation_caches(struct cpu *cpu, uint64_t, int);
void alpha_invalidate_code_translation(struct cpu *cpu, uint64_t, int);
void alpha_init_64bit_dummy_tables(struct cpu *cpu);
int alpha_run_instr(struct cpu *cpu);
int alpha_memory_rw(struct cpu *cpu, struct memory *mem, uint64_t vaddr,
	unsigned char *data, size_t len, int writeflag, int cache_flags);
int alpha_cpu_family_init(struct cpu_family *);

/*  cpu_alpha_palcode.c:  */
void alpha_palcode_name(uint32_t palcode, char *buf, size_t buflen);
void alpha_palcode(struct cpu *cpu, uint32_t palcode);

/*  memory_alpha.c:  */
int alpha_translate_v2p(struct cpu *cpu, uint64_t vaddr,
	uint64_t *return_addr, int flags);


#endif	/*  CPU_ALPHA_H  */
