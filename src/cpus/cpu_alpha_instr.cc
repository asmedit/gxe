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
 *  Alpha instructions.
 *
 *  Individual functions should keep track of cpu->n_translated_instrs.
 *  (If no instruction was executed, then it should be decreased. If, say, 4
 *  instructions were combined into one function and executed, then it should
 *  be increased by 3.)
 */


#include "float_emul.h"


/*
 *  nop:  Do nothing.
 */
X(nop)
{
}


/*
 *  call_pal:  PALcode call
 *
 *  arg[0] = pal nr
 */
X(call_pal)
{
	/*  Synchronize PC first:  */
	uint64_t old_pc, low_pc = ((size_t)ic - (size_t)
	    cpu->cd.alpha.cur_ic_page) / sizeof(struct alpha_instr_call);
	cpu->pc &= ~((ALPHA_IC_ENTRIES_PER_PAGE-1) <<
	    ALPHA_INSTR_ALIGNMENT_SHIFT);
	cpu->pc += (low_pc << ALPHA_INSTR_ALIGNMENT_SHIFT);
	old_pc = cpu->pc;

	alpha_palcode(cpu, ic->arg[0]);

	if (!cpu->running) {
		cpu->n_translated_instrs --;
		cpu->cd.alpha.next_ic = &nothing_call;
	} else if (cpu->pc != old_pc) {
		/*  The PC value was changed by the palcode call.  */
		/*  Find the new physical page and update the translation
		    pointers:  */
		alpha_pc_to_pointers(cpu);
	}
}


/*
 *  jsr:  Jump to SubRoutine
 *
 *  arg[0] = ptr to uint64_t where to store return PC
 *  arg[1] = ptr to uint64_t of new PC
 */
X(jsr)
{
	uint64_t old_pc = cpu->pc, low_pc;
	uint64_t mask_within_page = ((ALPHA_IC_ENTRIES_PER_PAGE-1)
	    << ALPHA_INSTR_ALIGNMENT_SHIFT) |
	    ((1 << ALPHA_INSTR_ALIGNMENT_SHIFT) - 1);

	low_pc = ((size_t)ic - (size_t)
	    cpu->cd.alpha.cur_ic_page) / sizeof(struct alpha_instr_call);
	cpu->pc &= ~((ALPHA_IC_ENTRIES_PER_PAGE-1)
	    << ALPHA_INSTR_ALIGNMENT_SHIFT);
	cpu->pc += (low_pc << ALPHA_INSTR_ALIGNMENT_SHIFT) + 4;

	*((int64_t *)ic->arg[0]) = cpu->pc;
	cpu->pc = *((int64_t *)ic->arg[1]);

	/*
	 *  If this is a jump/return into the same code page as we were
	 *  already in, then just set cpu->cd.alpha.next_ic.
	 */
	if ((old_pc & ~mask_within_page) == (cpu->pc & ~mask_within_page)) {
		cpu->cd.alpha.next_ic = cpu->cd.alpha.cur_ic_page +
		    ((cpu->pc & mask_within_page) >> 2);
	} else {
		/*  Find the new physical page and update pointers:  */
		alpha_pc_to_pointers(cpu);
	}
}


/*
 *  jsr_trace:  Jump to SubRoutine (with function call trace enabled)
 *
 *  Arguments same as for jsr.
 */
X(jsr_trace)
{
	cpu_functioncall_trace(cpu, *((int64_t *)ic->arg[1]));
	instr(jsr)(cpu, ic);
}


/*
 *  jsr_0:  JSR/RET, don't store return PC.
 *
 *  arg[0] = ignored
 *  arg[1] = ptr to uint64_t of new PC
 */
X(jsr_0)
{
	uint64_t old_pc = cpu->pc;
	uint64_t mask_within_page = ((ALPHA_IC_ENTRIES_PER_PAGE-1)
	    << ALPHA_INSTR_ALIGNMENT_SHIFT)
	    | ((1 << ALPHA_INSTR_ALIGNMENT_SHIFT) - 1);

	cpu->pc = *((int64_t *)ic->arg[1]);

	/*
	 *  If this is a jump/return into the same code page as we were
	 *  already in, then just set cpu->cd.alpha.next_ic.
	 */
	if ((old_pc & ~mask_within_page) == (cpu->pc & ~mask_within_page)) {
		cpu->cd.alpha.next_ic = cpu->cd.alpha.cur_ic_page +
		    ((cpu->pc & mask_within_page) >> 2);
	} else {
		/*  Find the new physical page and update pointers:  */
		alpha_pc_to_pointers(cpu);
	}
}


/*
 *  jsr_0_trace:  JSR/RET (with function call trace enabled)
 *
 *  Arguments same as for jsr_0.
 */
X(jsr_0_trace)
{
	cpu_functioncall_trace_return(cpu);
	instr(jsr_0)(cpu, ic);
}


/*
 *  br:  Branch (to a different translated page)
 *
 *  arg[0] = relative offset (as an int32_t)
 */
X(br)
{
	uint64_t low_pc;

	/*  Calculate new PC from this instruction + arg[0]  */
	low_pc = ((size_t)ic - (size_t)
	    cpu->cd.alpha.cur_ic_page) / sizeof(struct alpha_instr_call);
	cpu->pc &= ~((ALPHA_IC_ENTRIES_PER_PAGE-1)
	    << ALPHA_INSTR_ALIGNMENT_SHIFT);
	cpu->pc += (low_pc << ALPHA_INSTR_ALIGNMENT_SHIFT);
	cpu->pc += (int32_t)ic->arg[0];

	/*  Find the new physical page and update the translation pointers:  */
	alpha_pc_to_pointers(cpu);
}


/*
 *  br:  Branch (to a different translated page), write return address
 *
 *  arg[0] = relative offset (as an int32_t)
 *  arg[1] = pointer to uint64_t where to write return address
 */
X(br_return)
{
	uint64_t low_pc;

	/*  Calculate new PC from this instruction + arg[0]  */
	low_pc = ((size_t)ic - (size_t)
	    cpu->cd.alpha.cur_ic_page) / sizeof(struct alpha_instr_call);
	cpu->pc &= ~((ALPHA_IC_ENTRIES_PER_PAGE-1)
	    << ALPHA_INSTR_ALIGNMENT_SHIFT);
	cpu->pc += (low_pc << ALPHA_INSTR_ALIGNMENT_SHIFT);

	/*  ... but first, save away the return address:  */
	*((int64_t *)ic->arg[1]) = cpu->pc + 4;

	cpu->pc += (int32_t)ic->arg[0];

	/*  Find the new physical page and update the translation pointers:  */
	alpha_pc_to_pointers(cpu);
}


/*
 *  beq:  Branch (to a different translated page) if Equal
 *
 *  arg[0] = relative offset (as an int32_t)
 *  arg[1] = pointer to int64_t register
 */
X(beq)
{
	if (*((int64_t *)ic->arg[1]) == 0)
		instr(br)(cpu, ic);
}


/*
 *  blbs:  Branch (to a different translated page) if Low Bit Set
 *
 *  arg[0] = relative offset (as an int32_t)
 *  arg[1] = pointer to int64_t register
 */
X(blbs)
{
	if (*((int64_t *)ic->arg[1]) & 1)
		instr(br)(cpu, ic);
}


/*
 *  blbc:  Branch (to a different translated page) if Low Bit Clear
 *
 *  arg[0] = relative offset (as an int32_t)
 *  arg[1] = pointer to int64_t register
 */
X(blbc)
{
	if (!(*((int64_t *)ic->arg[1]) & 1))
		instr(br)(cpu, ic);
}


/*
 *  bne:  Branch (to a different translated page) if Not Equal
 *
 *  arg[0] = relative offset (as an int32_t)
 *  arg[1] = pointer to int64_t register
 */
X(bne)
{
	if (*((int64_t *)ic->arg[1]) != 0)
		instr(br)(cpu, ic);
}


/*
 *  ble:  Branch (to a different translated page) if Less or Equal
 *
 *  arg[0] = relative offset (as an int32_t)
 *  arg[1] = pointer to int64_t register
 */
X(ble)
{
	if (*((int64_t *)ic->arg[1]) <= 0)
		instr(br)(cpu, ic);
}


/*
 *  blt:  Branch (to a different translated page) if Less Than
 *
 *  arg[0] = relative offset (as an int32_t)
 *  arg[1] = pointer to int64_t register
 */
X(blt)
{
	if (*((int64_t *)ic->arg[1]) < 0)
		instr(br)(cpu, ic);
}


/*
 *  bge:  Branch (to a different translated page) if Greater or Equal
 *
 *  arg[0] = relative offset (as an int32_t)
 *  arg[1] = pointer to int64_t register
 */
X(bge)
{
	if (*((int64_t *)ic->arg[1]) >= 0)
		instr(br)(cpu, ic);
}


/*
 *  bgt:  Branch (to a different translated page) if Greater Than
 *
 *  arg[0] = relative offset (as an int32_t)
 *  arg[1] = pointer to int64_t register
 */
X(bgt)
{
	if (*((int64_t *)ic->arg[1]) > 0)
		instr(br)(cpu, ic);
}


/*
 *  br_samepage:  Branch (to within the same translated page)
 *
 *  arg[0] = pointer to new alpha_instr_call
 */
X(br_samepage)
{
	cpu->cd.alpha.next_ic = (struct alpha_instr_call *) ic->arg[0];
}


/*
 *  br_return_samepage:  Branch (to within the same translated page),
 *                       and save return address
 *
 *  arg[0] = pointer to new alpha_instr_call
 *  arg[1] = pointer to uint64_t where to store return address
 */
X(br_return_samepage)
{
	uint64_t low_pc;

	low_pc = ((size_t)ic - (size_t)
	    cpu->cd.alpha.cur_ic_page) / sizeof(struct alpha_instr_call);
	cpu->pc &= ~((ALPHA_IC_ENTRIES_PER_PAGE-1)
	    << ALPHA_INSTR_ALIGNMENT_SHIFT);
	cpu->pc += (low_pc << ALPHA_INSTR_ALIGNMENT_SHIFT);
	*((int64_t *)ic->arg[1]) = cpu->pc + 4;

	cpu->cd.alpha.next_ic = (struct alpha_instr_call *) ic->arg[0];
}


/*
 *  beq_samepage:  Branch (to within the same translated page) if Equal
 *
 *  arg[0] = pointer to new alpha_instr_call
 *  arg[1] = pointer to int64_t register
 */
X(beq_samepage)
{
	if (*((int64_t *)ic->arg[1]) == 0)
		instr(br_samepage)(cpu, ic);
}


/*
 *  blbs_samepage:  Branch (to within the same translated page) if Low Bit Set
 *
 *  arg[0] = pointer to new alpha_instr_call
 *  arg[1] = pointer to int64_t register
 */
X(blbs_samepage)
{
	if (*((int64_t *)ic->arg[1]) & 1)
		instr(br_samepage)(cpu, ic);
}


/*
 *  blbc_samepage:  Branch (to within the same translated page) if Low Bit Clear
 *
 *  arg[0] = pointer to new alpha_instr_call
 *  arg[1] = pointer to int64_t register
 */
X(blbc_samepage)
{
	if (!(*((int64_t *)ic->arg[1]) & 1))
		instr(br_samepage)(cpu, ic);
}


/*
 *  bne_samepage:  Branch (to within the same translated page) if Not Equal
 *
 *  arg[0] = pointer to new alpha_instr_call
 *  arg[1] = pointer to int64_t register
 */
X(bne_samepage)
{
	if (*((int64_t *)ic->arg[1]) != 0)
		instr(br_samepage)(cpu, ic);
}


/*
 *  ble_samepage:  Branch (to within the same translated page) if Less or Equal
 *
 *  arg[0] = pointer to new alpha_instr_call
 *  arg[1] = pointer to int64_t register
 */
X(ble_samepage)
{
	if (*((int64_t *)ic->arg[1]) <= 0)
		instr(br_samepage)(cpu, ic);
}


/*
 *  blt_samepage:  Branch (to within the same translated page) if Less Than
 *
 *  arg[0] = pointer to new alpha_instr_call
 *  arg[1] = pointer to int64_t register
 */
X(blt_samepage)
{
	if (*((int64_t *)ic->arg[1]) < 0)
		instr(br_samepage)(cpu, ic);
}


/*
 *  bge_samepage:  Branch (to within the same translated page)
 *		   if Greater or Equal
 *
 *  arg[0] = pointer to new alpha_instr_call
 *  arg[1] = pointer to int64_t register
 */
X(bge_samepage)
{
	if (*((int64_t *)ic->arg[1]) >= 0)
		instr(br_samepage)(cpu, ic);
}


/*
 *  bgt_samepage:  Branch (to within the same translated page) if Greater Than
 *
 *  arg[0] = pointer to new alpha_instr_call
 *  arg[1] = pointer to int64_t register
 */
X(bgt_samepage)
{
	if (*((int64_t *)ic->arg[1]) > 0)
		instr(br_samepage)(cpu, ic);
}


/*
 *  cvttq/c:  Convert floating point to quad.
 *
 *  arg[0] = pointer to rc  (destination integer)
 *  arg[2] = pointer to rb  (source float)
 */
X(cvttq_c)
{
	struct ieee_float_value fb;
	ieee_interpret_float_value(reg(ic->arg[2]), &fb, IEEE_FMT_D);
	reg(ic->arg[0]) = fb.nan? 0 : fb.f;
}


/*
 *  cvtqt:  Convert quad to floating point.
 *
 *  arg[0] = pointer to rc  (destination float)
 *  arg[2] = pointer to rb  (source quad integer)
 */
X(cvtqt)
{
	reg(ic->arg[0]) = ieee_store_float_value(reg(ic->arg[2]),
	    IEEE_FMT_D, 0);
}


/*
 *  fabs, fneg:  Floating point absolute value, or negation.
 *
 *  arg[0] = pointer to rc  (destination float)
 *  arg[2] = pointer to rb  (source quad integer)
 */
X(fabs)
{
	reg(ic->arg[0]) = reg(ic->arg[2]) & 0x7fffffffffffffffULL;
}
X(fneg)
{
	reg(ic->arg[0]) = reg(ic->arg[2]) ^ 0x8000000000000000ULL;
}


/*
 *  addt, subt, mult, divt:  Floating point arithmetic.
 *
 *  arg[0] = pointer to rc  (destination)
 *  arg[1] = pointer to ra  (source)
 *  arg[2] = pointer to rb  (source)
 */
X(addt)
{
	struct ieee_float_value fa, fb;
	double res;
	ieee_interpret_float_value(reg(ic->arg[1]), &fa, IEEE_FMT_D);
	ieee_interpret_float_value(reg(ic->arg[2]), &fb, IEEE_FMT_D);
	if (fa.nan | fb.nan)
		res = 0.0;
	else
		res = fa.f + fb.f;
	reg(ic->arg[0]) = ieee_store_float_value(res,
	    IEEE_FMT_D, fa.nan | fb.nan);
}
X(subt)
{
	struct ieee_float_value fa, fb;
	double res;
	ieee_interpret_float_value(reg(ic->arg[1]), &fa, IEEE_FMT_D);
	ieee_interpret_float_value(reg(ic->arg[2]), &fb, IEEE_FMT_D);
	if (fa.nan | fb.nan)
		res = 0.0;
	else
		res = fa.f - fb.f;
	reg(ic->arg[0]) = ieee_store_float_value(res,
	    IEEE_FMT_D, fa.nan | fb.nan);
}
X(mult)
{
	struct ieee_float_value fa, fb;
	double res;
	ieee_interpret_float_value(reg(ic->arg[1]), &fa, IEEE_FMT_D);
	ieee_interpret_float_value(reg(ic->arg[2]), &fb, IEEE_FMT_D);
	if (fa.nan | fb.nan)
		res = 0.0;
	else
		res = fa.f * fb.f;
	reg(ic->arg[0]) = ieee_store_float_value(res,
	    IEEE_FMT_D, fa.nan | fb.nan);
}
X(divt)
{
	struct ieee_float_value fa, fb;
	double res;
	ieee_interpret_float_value(reg(ic->arg[1]), &fa, IEEE_FMT_D);
	ieee_interpret_float_value(reg(ic->arg[2]), &fb, IEEE_FMT_D);
	if (fa.nan | fb.nan || fb.f == 0)
		res = 0.0;
	else
		res = fa.f / fb.f;
	reg(ic->arg[0]) = ieee_store_float_value(res,
	    IEEE_FMT_D, fa.nan | fb.nan || fb.f == 0);
}
X(cmpteq)
{
	struct ieee_float_value fa, fb;
	int res = 0;
	ieee_interpret_float_value(reg(ic->arg[1]), &fa, IEEE_FMT_D);
	ieee_interpret_float_value(reg(ic->arg[2]), &fb, IEEE_FMT_D);
	if (fa.nan | fb.nan)
		res = 0;
	else
		res = fa.f == fb.f;
	reg(ic->arg[0]) = res;
}
X(cmptlt)
{
	struct ieee_float_value fa, fb;
	int res = 0;
	ieee_interpret_float_value(reg(ic->arg[1]), &fa, IEEE_FMT_D);
	ieee_interpret_float_value(reg(ic->arg[2]), &fb, IEEE_FMT_D);
	if (fa.nan | fb.nan)
		res = 0;
	else
		res = fa.f < fb.f;
	reg(ic->arg[0]) = res;
}
X(cmptle)
{
	struct ieee_float_value fa, fb;
	int res = 0;
	ieee_interpret_float_value(reg(ic->arg[1]), &fa, IEEE_FMT_D);
	ieee_interpret_float_value(reg(ic->arg[2]), &fb, IEEE_FMT_D);
	if (fa.nan | fb.nan)
		res = 0;
	else
		res = fa.f <= fb.f;
	reg(ic->arg[0]) = res;
}


/*
 *  implver:  Return CPU implver value.
 *
 *  arg[0] = pointer to destination uint64_t
 */
X(implver)
{
	reg(ic->arg[0]) = cpu->cd.alpha.cpu_type.implver;
}


/*
 *  mull, mull_imm:  Signed Multiply 32x32 => 32.
 *
 *  arg[0] = pointer to destination uint64_t
 *  arg[1] = pointer to source uint64_t
 *  arg[2] = pointer to source uint64_t or immediate
 */
X(mull)
{
	int32_t a = reg(ic->arg[1]);
	int32_t b = reg(ic->arg[2]);
	reg(ic->arg[0]) = (int64_t)(int32_t)(a * b);
}
X(mull_imm)
{
	int32_t a = reg(ic->arg[1]);
	int32_t b = ic->arg[2];
	reg(ic->arg[0]) = (int64_t)(int32_t)(a * b);
}


/*
 *  mulq, mulq_imm:  Unsigned Multiply 64x64 => 64.
 *
 *  arg[0] = pointer to destination uint64_t
 *  arg[1] = pointer to source uint64_t
 *  arg[2] = pointer to source uint64_t or immediate
 */
X(mulq)
{
	reg(ic->arg[0]) = reg(ic->arg[1]) * reg(ic->arg[2]);
}
X(mulq_imm)
{
	reg(ic->arg[0]) = reg(ic->arg[1]) * ic->arg[2];
}


/*
 *  umulh:  Unsigned Multiply 64x64 => 128. Store high part in dest reg.
 *
 *  arg[0] = pointer to destination uint64_t
 *  arg[1] = pointer to source uint64_t
 *  arg[2] = pointer to source uint64_t
 */
X(umulh)
{
	uint64_t reshi = 0, reslo = 0;
	uint64_t s1 = reg(ic->arg[1]), s2 = reg(ic->arg[2]);
	int i, bit;

	for (i=0; i<64; i++) {
		bit = (s1 & 0x8000000000000000ULL)? 1 : 0;
		s1 <<= 1;

		/*  If bit in s1 set, then add s2 to reshi/lo:  */
		if (bit) {
			uint64_t old_reslo = reslo;
			reslo += s2;
			if (reslo < old_reslo)
				reshi ++;
		}

		if (i != 63) {
			reshi <<= 1;
			reshi += (reslo & 0x8000000000000000ULL? 1 : 0);
			reslo <<= 1;
		}
	}

	reg(ic->arg[0]) = reshi;
}


/*
 *  lda:  Load address.
 *
 *  arg[0] = pointer to destination uint64_t
 *  arg[1] = pointer to source uint64_t
 *  arg[2] = offset (possibly as an int32_t)
 */
X(lda)
{
	reg(ic->arg[0]) = reg(ic->arg[1]) + (int64_t)(int32_t)ic->arg[2];
}


/*
 *  lda_0:  Load address compared to the zero register.
 *
 *  arg[0] = pointer to destination uint64_t
 *  arg[1] = ignored
 *  arg[2] = offset (possibly as an int32_t)
 */
X(lda_0)
{
	reg(ic->arg[0]) = (int64_t)(int32_t)ic->arg[2];
}


/*
 *  clear:  Clear a 64-bit register.
 *
 *  arg[0] = pointer to destination uint64_t
 */
X(clear)
{
	reg(ic->arg[0]) = 0;
}


/*
 *  rdcc:  Read the Cycle Counter into a 64-bit register.
 *
 *  arg[0] = pointer to destination uint64_t
 */
X(rdcc)
{
	reg(ic->arg[0]) = cpu->cd.alpha.pcc;

	/*  TODO: actually keep the pcc updated!  */
	cpu->cd.alpha.pcc += 20;
}


#include "tmp_alpha_misc.cc"


/*****************************************************************************/


X(end_of_page)
{
	/*  Update the PC:  (offset 0, but on the next page)  */
	cpu->pc &= ~((ALPHA_IC_ENTRIES_PER_PAGE-1)
	    << ALPHA_INSTR_ALIGNMENT_SHIFT);
	cpu->pc += (ALPHA_IC_ENTRIES_PER_PAGE
	    << ALPHA_INSTR_ALIGNMENT_SHIFT);

	/*  Find the new physical page and update the translation pointers:  */
	alpha_pc_to_pointers(cpu);

	/*  end_of_page doesn't count as an executed instruction:  */
	cpu->n_translated_instrs --;
}


/*****************************************************************************/


/*
 *  alpha_instr_to_be_translated():
 *
 *  Translate an instruction word into an alpha_instr_call. ic is filled in with
 *  valid data for the translated instruction, or a "nothing" instruction if
 *  there was a translation failure. The newly translated instruction is then
 *  executed.
 */
X(to_be_translated)
{
	uint64_t addr, low_pc;
	uint32_t iword;
	unsigned char *page;
	unsigned char ib[4];
	void (*samepage_function)(struct cpu *, struct alpha_instr_call *);
	int opcode, ra, rb, func, rc, imm, load, loadstore_type, fp, llsc;

	/*  Figure out the (virtual) address of the instruction:  */
	low_pc = ((size_t)ic - (size_t)cpu->cd.alpha.cur_ic_page)
	    / sizeof(struct alpha_instr_call);
	addr = cpu->pc & ~((ALPHA_IC_ENTRIES_PER_PAGE-1) <<
	    ALPHA_INSTR_ALIGNMENT_SHIFT);
	addr += (low_pc << ALPHA_INSTR_ALIGNMENT_SHIFT);
	addr &= ~((1 << ALPHA_INSTR_ALIGNMENT_SHIFT) - 1);
	cpu->pc = addr;

	/*  Read the instruction word from memory:  */
	{
		const uint32_t mask1 = (1 << DYNTRANS_L1N) - 1;
		const uint32_t mask2 = (1 << DYNTRANS_L2N) - 1;
		const uint32_t mask3 = (1 << DYNTRANS_L3N) - 1;
		uint32_t x1 = (addr >> (64-DYNTRANS_L1N)) & mask1;
		uint32_t x2 = (addr >> (64-DYNTRANS_L1N-DYNTRANS_L2N)) & mask2;
		uint32_t x3 = (addr >> (64-DYNTRANS_L1N-DYNTRANS_L2N-
		    DYNTRANS_L3N)) & mask3;
		struct DYNTRANS_L2_64_TABLE *l2 = cpu->cd.alpha.l1_64[x1];
		struct DYNTRANS_L3_64_TABLE *l3 = l2->l3[x2];
		page = l3->host_load[x3];
	}

	if (page != NULL) {
		/*  fatal("TRANSLATION HIT!\n");  */
		memcpy(ib, page + (addr & 8191), sizeof(ib));
	} else {
		/*  fatal("TRANSLATION MISS!\n");  */
		if (!cpu->memory_rw(cpu, cpu->mem, addr, &ib[0],
		    sizeof(ib), MEM_READ, CACHE_INSTRUCTION)) {
			fatal("to_be_translated(): read failed: TODO\n");
			goto bad;
		}
	}

	/*  Alpha instruction words are always little-endian. Convert
	    to host order:  */
	{
		uint32_t *p = (uint32_t *) ib;
		iword = LE32_TO_HOST( *p );
	}

#define DYNTRANS_TO_BE_TRANSLATED_HEAD
#include "cpu_dyntrans.cc"
#undef	DYNTRANS_TO_BE_TRANSLATED_HEAD


	opcode = (iword >> 26) & 63;
	ra = (iword >> 21) & 31;
	rb = (iword >> 16) & 31;
	func = (iword >> 5) & 0x7ff;
	rc = iword & 31;
	imm = iword & 0xffff;

	switch (opcode) {
	case 0x00:						/*  CALL_PAL  */
		ic->f = instr(call_pal);
		ic->arg[0] = (size_t) (iword & 0x3ffffff);
		break;
	case 0x08:						/*  LDA  */
	case 0x09:						/*  LDAH  */
		if (ra == ALPHA_ZERO) {
			ic->f = instr(nop);
			break;
		}
		/*  TODO: A special case which is common is to add or subtract
		    a small offset from sp.  */
		ic->f = instr(lda);
		ic->arg[0] = (size_t) &cpu->cd.alpha.r[ra];
		ic->arg[1] = (size_t) &cpu->cd.alpha.r[rb];
		if (rb == ALPHA_ZERO)
			ic->f = instr(lda_0);
		ic->arg[2] = (ssize_t)(int16_t)imm;
		if (opcode == 0x09)
			ic->arg[2] <<= 16;
		break;
	case 0x0b:						/*  LDQ_U  */
	case 0x0f:						/*  STQ_U  */
		if (ra == ALPHA_ZERO && opcode == 0x0b) {
			ic->f = instr(nop);
			break;
		}
		if (opcode == 0x0b)
			ic->f = instr(ldq_u);
		else
			ic->f = instr(stq_u);
		ic->arg[0] = (size_t) &cpu->cd.alpha.r[ra];
		ic->arg[1] = (size_t) &cpu->cd.alpha.r[rb];
		ic->arg[2] = (ssize_t)(int16_t)imm;
		break;
	case 0x0a:
	case 0x0c:
	case 0x0d:
	case 0x0e:
	case 0x22:
	case 0x23:
	case 0x26:
	case 0x27:
	case 0x28:
	case 0x29:
	case 0x2a:
	case 0x2b:
	case 0x2c:
	case 0x2d:
	case 0x2e:
	case 0x2f:
		loadstore_type = 0; fp = 0; load = 0; llsc = 0;
		switch (opcode) {
		case 0x0a: loadstore_type = 0; load = 1; break;	/*  ldbu  */
		case 0x0c: loadstore_type = 1; load = 1; break;	/*  ldwu  */
		case 0x0d: loadstore_type = 1; break;		/*  stw  */
		case 0x0e: loadstore_type = 0; break;		/*  stb  */
		case 0x22: loadstore_type = 2; load = 1; fp = 1; break; /*lds*/
		case 0x23: loadstore_type = 3; load = 1; fp = 1; break; /*ldt*/
		case 0x26: loadstore_type = 2; fp = 1; break;	/*  sts  */
		case 0x27: loadstore_type = 3; fp = 1; break;	/*  stt  */
		case 0x28: loadstore_type = 2; load = 1; break;	/*  ldl  */
		case 0x29: loadstore_type = 3; load = 1; break;	/*  ldq  */
		case 0x2a: loadstore_type = 2; load = llsc = 1; break;/* ldl_l*/
		case 0x2b: loadstore_type = 3; load = llsc = 1; break;/* ldq_l*/
		case 0x2c: loadstore_type = 2; break;		/*  stl  */
		case 0x2d: loadstore_type = 3; break;		/*  stq  */
		case 0x2e: loadstore_type = 2; llsc = 1; break;	/*  stl_c  */
		case 0x2f: loadstore_type = 3; llsc = 1; break;	/*  stq_c  */
		}
		ic->f = alpha_loadstore[
		    loadstore_type + (imm==0? 4 : 0) + 8 * load
		    + 16 * llsc];
		/*  Load to the zero register is treated as a prefetch
		    hint. It is ignored here.  */
		if (load && ra == ALPHA_ZERO) {
			ic->f = instr(nop);
			break;
		}
		if (fp)
			ic->arg[0] = (size_t) &cpu->cd.alpha.f[ra];
		else
			ic->arg[0] = (size_t) &cpu->cd.alpha.r[ra];
		ic->arg[1] = (size_t) &cpu->cd.alpha.r[rb];
		ic->arg[2] = (ssize_t)(int16_t)imm;
		break;
	case 0x10:
		if (rc == ALPHA_ZERO) {
			ic->f = instr(nop);
			break;
		}
		ic->arg[0] = (size_t) &cpu->cd.alpha.r[rc];
		ic->arg[1] = (size_t) &cpu->cd.alpha.r[ra];
		if (func & 0x80)
			ic->arg[2] = (size_t)((rb << 3) + (func >> 8));
		else
			ic->arg[2] = (size_t) &cpu->cd.alpha.r[rb];
		switch (func & 0xff) {
		case 0x00: ic->f = instr(addl); break;
		case 0x02: ic->f = instr(s4addl); break;
		case 0x09: ic->f = instr(subl); break;
		case 0x0b: ic->f = instr(s4subl); break;
		case 0x0f: ic->f = instr(cmpbge); break;
		case 0x12: ic->f = instr(s8addl); break;
		case 0x1b: ic->f = instr(s8subl); break;
		case 0x1d: ic->f = instr(cmpult); break;
		case 0x20: ic->f = instr(addq); break;
		case 0x22: ic->f = instr(s4addq); break;
		case 0x29: ic->f = instr(subq); break;
		case 0x2b: ic->f = instr(s4subq); break;
		case 0x2d: ic->f = instr(cmpeq); break;
		case 0x32: ic->f = instr(s8addq); break;
		case 0x3b: ic->f = instr(s8subq); break;
		case 0x3d: ic->f = instr(cmpule); break;
		case 0x4d: ic->f = instr(cmplt); break;
		// case 0x69: ic->f = instr(subq_v); break;
		case 0x6d: ic->f = instr(cmple); break;

		case 0x80: ic->f = instr(addl_imm); break;
		case 0x82: ic->f = instr(s4addl_imm); break;
		case 0x89: ic->f = instr(subl_imm); break;
		case 0x8b: ic->f = instr(s4subl_imm); break;
		case 0x8f: ic->f = instr(cmpbge_imm); break;
		case 0x92: ic->f = instr(s8addl_imm); break;
		case 0x9b: ic->f = instr(s8subl_imm); break;
		case 0x9d: ic->f = instr(cmpult_imm); break;
		case 0xa0: ic->f = instr(addq_imm); break;
		case 0xa2: ic->f = instr(s4addq_imm); break;
		case 0xa9: ic->f = instr(subq_imm); break;
		case 0xab: ic->f = instr(s4subq_imm); break;
		case 0xad: ic->f = instr(cmpeq_imm); break;
		case 0xb2: ic->f = instr(s8addq_imm); break;
		case 0xbb: ic->f = instr(s8subq_imm); break;
		case 0xbd: ic->f = instr(cmpule_imm); break;
		case 0xcd: ic->f = instr(cmplt_imm); break;
		case 0xed: ic->f = instr(cmple_imm); break;

		default:if (!cpu->translation_readahead)
				fatal("[ Alpha: unimplemented function 0x%03x "
				    "for opcode 0x%02x ]\n", func, opcode);
			goto bad;
		}
		break;
	case 0x11:
		if (rc == ALPHA_ZERO) {
			ic->f = instr(nop);
			break;
		}
		ic->arg[0] = (size_t) &cpu->cd.alpha.r[rc];
		ic->arg[1] = (size_t) &cpu->cd.alpha.r[ra];
		if (func & 0x80)
			ic->arg[2] = (size_t)((rb << 3) + (func >> 8));
		else
			ic->arg[2] = (size_t) &cpu->cd.alpha.r[rb];
		switch (func & 0xff) {
		case 0x00: ic->f = instr(and); break;
		case 0x08: ic->f = instr(andnot); break;
		case 0x14: ic->f = instr(cmovlbs); break;
		case 0x16: ic->f = instr(cmovlbc); break;
		case 0x20: ic->f = instr(or);
			   if (ra == ALPHA_ZERO || rb == ALPHA_ZERO) {
				if (ra == ALPHA_ZERO)
					ra = rb;
				ic->f = alpha_mov_r_r[ra + rc*32];
			   }
			   break;
		case 0x24: ic->f = instr(cmoveq); break;
		case 0x26: ic->f = instr(cmovne); break;
		case 0x28: ic->f = instr(ornot); break;
		case 0x40: ic->f = instr(xor); break;
		case 0x44: ic->f = instr(cmovlt); break;
		case 0x46: ic->f = instr(cmovge); break;
		case 0x48: ic->f = instr(xornot); break;
		case 0x64: ic->f = instr(cmovle); break;
		case 0x66: ic->f = instr(cmovgt); break;
		case 0x80: ic->f = instr(and_imm); break;
		case 0x88: ic->f = instr(andnot_imm); break;
		case 0x94: ic->f = instr(cmovlbs_imm); break;
		case 0x96: ic->f = instr(cmovlbc_imm); break;
		case 0xa0: ic->f = instr(or_imm); break;
		case 0xa4: ic->f = instr(cmoveq_imm); break;
		case 0xa6: ic->f = instr(cmovne_imm); break;
		case 0xa8: ic->f = instr(ornot_imm); break;
		case 0xc0: ic->f = instr(xor_imm); break;
		case 0xc4: ic->f = instr(cmovlt_imm); break;
		case 0xc6: ic->f = instr(cmovge_imm); break;
		case 0xc8: ic->f = instr(xornot_imm); break;
		case 0xe4: ic->f = instr(cmovle_imm); break;
		case 0xe6: ic->f = instr(cmovgt_imm); break;
		case 0xec: ic->f = instr(implver); break;
		default:if (!cpu->translation_readahead)
				fatal("[ Alpha: unimplemented function 0x%03x "
				    "for opcode 0x%02x ]\n", func, opcode);
			goto bad;
		}
		break;
	case 0x12:
		if (rc == ALPHA_ZERO) {
			ic->f = instr(nop);
			break;
		}
		ic->arg[0] = (size_t) &cpu->cd.alpha.r[rc];
		ic->arg[1] = (size_t) &cpu->cd.alpha.r[ra];
		if (func & 0x80)
			ic->arg[2] = (size_t)((rb << 3) + (func >> 8));
		else
			ic->arg[2] = (size_t) &cpu->cd.alpha.r[rb];
		switch (func & 0xff) {
		case 0x02: ic->f = instr(mskbl); break;
		case 0x06: ic->f = instr(extbl); break;
		case 0x0b: ic->f = instr(insbl); break;
		case 0x12: ic->f = instr(mskwl); break;
		case 0x16: ic->f = instr(extwl); break;
		case 0x1b: ic->f = instr(inswl); break;
		case 0x22: ic->f = instr(mskll); break;
		case 0x26: ic->f = instr(extll); break;
		case 0x2b: ic->f = instr(insll); break;
		case 0x30: ic->f = instr(zap); break;
		case 0x31: ic->f = instr(zapnot); break;
		case 0x32: ic->f = instr(mskql); break;
		case 0x34: ic->f = instr(srl); break;
		case 0x36: ic->f = instr(extql); break;
		case 0x39: ic->f = instr(sll); break;
		case 0x3b: ic->f = instr(insql); break;
		case 0x3c: ic->f = instr(sra); break;
		case 0x52: ic->f = instr(mskwh); break;
		case 0x57: ic->f = instr(inswh); break;
		case 0x5a: ic->f = instr(extwh); break;
		case 0x62: ic->f = instr(msklh); break;
		case 0x67: ic->f = instr(inslh); break;
		case 0x6a: ic->f = instr(extlh); break;
		case 0x72: ic->f = instr(mskqh); break;
		case 0x77: ic->f = instr(insqh); break;
		case 0x7a: ic->f = instr(extqh); break;
		case 0x82: ic->f = instr(mskbl_imm); break;
		case 0x86: ic->f = instr(extbl_imm); break;
		case 0x8b: ic->f = instr(insbl_imm); break;
		case 0x92: ic->f = instr(mskwl_imm); break;
		case 0x96: ic->f = instr(extwl_imm); break;
		case 0x9b: ic->f = instr(inswl_imm); break;
		case 0xa2: ic->f = instr(mskll_imm); break;
		case 0xa6: ic->f = instr(extll_imm); break;
		case 0xab: ic->f = instr(insll_imm); break;
		case 0xb0: ic->f = instr(zap_imm); break;
		case 0xb1: ic->f = instr(zapnot_imm); break;
		case 0xb2: ic->f = instr(mskql_imm); break;
		case 0xb4: ic->f = instr(srl_imm); break;
		case 0xb6: ic->f = instr(extql_imm); break;
		case 0xb9: ic->f = instr(sll_imm); break;
		case 0xbb: ic->f = instr(insql_imm); break;
		case 0xbc: ic->f = instr(sra_imm); break;
		case 0xd2: ic->f = instr(mskwh_imm); break;
		case 0xd7: ic->f = instr(inswh_imm); break;
		case 0xda: ic->f = instr(extwh_imm); break;
		case 0xe2: ic->f = instr(msklh_imm); break;
		case 0xe7: ic->f = instr(inslh_imm); break;
		case 0xea: ic->f = instr(extlh_imm); break;
		case 0xf2: ic->f = instr(mskqh_imm); break;
		case 0xf7: ic->f = instr(insqh_imm); break;
		case 0xfa: ic->f = instr(extqh_imm); break;
		default:if (!cpu->translation_readahead)
				fatal("[ Alpha: unimplemented function 0x%03x "
				    "for opcode 0x%02x ]\n", func, opcode);
			goto bad;
		}
		break;
	case 0x13:
		if (rc == ALPHA_ZERO) {
			ic->f = instr(nop);
			break;
		}
		ic->arg[0] = (size_t) &cpu->cd.alpha.r[rc];
		ic->arg[1] = (size_t) &cpu->cd.alpha.r[ra];
		if (func & 0x80)
			ic->arg[2] = (size_t)((rb << 3) + (func >> 8));
		else
			ic->arg[2] = (size_t) &cpu->cd.alpha.r[rb];
		// TODO: mulq/v etc? overflow detection
		// bit 0..6 are function, but 7 is "imm" bit.
		switch (func & 0xff) {
		case 0x00: ic->f = instr(mull); break;
		case 0x20: ic->f = instr(mulq); break;
		case 0x30: ic->f = instr(umulh); break;
		case 0x80: ic->f = instr(mull_imm); break;
		case 0xa0: ic->f = instr(mulq_imm); break;
		default:if (!cpu->translation_readahead)
				fatal("[ Alpha: unimplemented function 0x%03x "
				    "for opcode 0x%02x ]\n", func, opcode);
			goto bad;
		}
		break;
	case 0x16:
		if (rc == ALPHA_ZERO) {
			ic->f = instr(nop);
			break;
		}
		ic->arg[0] = (size_t) &cpu->cd.alpha.f[rc];
		ic->arg[1] = (size_t) &cpu->cd.alpha.f[ra];
		ic->arg[2] = (size_t) &cpu->cd.alpha.f[rb];
		switch (func & 0x7ff) {
		case 0x02f: ic->f = instr(cvttq_c); break;
		case 0x0a0: ic->f = instr(addt); break;
		case 0x0a1: ic->f = instr(subt); break;
		case 0x0a2: ic->f = instr(mult); break;
		case 0x0a3: ic->f = instr(divt); break;
		case 0x0a5: ic->f = instr(cmpteq); break;
		case 0x0a6: ic->f = instr(cmptlt); break;
		case 0x0a7: ic->f = instr(cmptle); break;
		case 0x0be: ic->f = instr(cvtqt); break;
		default:if (!cpu->translation_readahead)
				fatal("[ Alpha: unimplemented function 0x%03x "
				    "for opcode 0x%02x ]\n", func, opcode);
			goto bad;
		}
		break;
	case 0x17:
		if (rc == ALPHA_ZERO) {
			ic->f = instr(nop);
			break;
		}
		ic->arg[0] = (size_t) &cpu->cd.alpha.f[rc];
		ic->arg[1] = (size_t) &cpu->cd.alpha.f[ra];
		ic->arg[2] = (size_t) &cpu->cd.alpha.f[rb];
		switch (func & 0x7ff) {
		case 0x020:
			/*  fabs (or fclr):  */
			if (ra == 31 && rb == 31)
				ic->f = instr(clear);
			else
				ic->f = instr(fabs);
			break;
		case 0x021:
			ic->f = instr(fneg);
			break;
		default:if (!cpu->translation_readahead)
				fatal("[ Alpha: unimplemented function 0x%03x "
				    "for opcode 0x%02x ]\n", func, opcode);
			goto bad;
		}
		break;
	case 0x18:
		switch (iword & 0xffff) {
		case 0x4000:	/*  mb  */
		case 0x4400:	/*  wmb  */
			ic->f = instr(nop);
			break;
		case 0xc000:	/*  rdcc  ra  */
			if (ra == ALPHA_ZERO) {
				ic->f = instr(nop);
				break;
			}
			ic->arg[0] = (size_t) &cpu->cd.alpha.r[ra];
			ic->f = instr(rdcc);
			break;
		default:if (!cpu->translation_readahead)
				fatal("[ Alpha: unimplemented function 0x%03x "
				    "for opcode 0x%02x ]\n", func, opcode);
			goto bad;
		}
		break;
	case 0x1a:
		switch ((iword >> 14) & 3) {
		case 0:	/*  JMP  */
		case 1:	/*  JSR  */
		case 2:	/*  RET  */
			ic->arg[0] = (size_t) &cpu->cd.alpha.r[ra];
			ic->arg[1] = (size_t) &cpu->cd.alpha.r[rb];
			if (ra == ALPHA_ZERO) {
				if (cpu->machine->show_trace_tree &&
				    rb == ALPHA_RA)
					ic->f = instr(jsr_0_trace);
				else
					ic->f = instr(jsr_0);
			} else {
				if (cpu->machine->show_trace_tree)
					ic->f = instr(jsr_trace);
				else
					ic->f = instr(jsr);
			}
			break;
		default:if (!cpu->translation_readahead)
				fatal("[ Alpha: unimpl JSR type %i, ra=%i "
				    "rb=%i ]\n", ((iword >> 14) & 3), ra, rb);
			goto bad;
		}
		break;
	case 0x30:						/*  BR    */
	case 0x31:						/*  FBEQ  */
	case 0x34:						/*  BSR   */
	case 0x35:						/*  FBNE  */
	case 0x38:						/*  BLBC  */
	case 0x39:						/*  BEQ   */
	case 0x3a:						/*  BLT   */
	case 0x3b:						/*  BLE   */
	case 0x3c:						/*  BLBS  */
	case 0x3d:						/*  BNE   */
	case 0x3e:						/*  BGE   */
	case 0x3f:						/*  BGT   */
		/*  To avoid a GCC warning:  */
		samepage_function = instr(nop);
		fp = 0;
		switch (opcode) {
		case 0x30:
		case 0x34:
			ic->f = instr(br);
			samepage_function = instr(br_samepage);
			if (ra != ALPHA_ZERO) {
				ic->f = instr(br_return);
				samepage_function = instr(br_return_samepage);
			}
			break;
		case 0x38:
			ic->f = instr(blbc);
			samepage_function = instr(blbc_samepage);
			break;
		case 0x31:
			fp = 1;
		case 0x39:
			ic->f = instr(beq);
			samepage_function = instr(beq_samepage);
			break;
		case 0x3a:
			ic->f = instr(blt);
			samepage_function = instr(blt_samepage);
			break;
		case 0x3b:
			ic->f = instr(ble);
			samepage_function = instr(ble_samepage);
			break;
		case 0x3c:
			ic->f = instr(blbs);
			samepage_function = instr(blbs_samepage);
			break;
		case 0x35:
			fp = 1;
		case 0x3d:
			ic->f = instr(bne);
			samepage_function = instr(bne_samepage);
			break;
		case 0x3e:
			ic->f = instr(bge);
			samepage_function = instr(bge_samepage);
			break;
		case 0x3f:
			ic->f = instr(bgt);
			samepage_function = instr(bgt_samepage);
			break;
		}
		if (fp)
			ic->arg[1] = (size_t) &cpu->cd.alpha.f[ra];
		else
			ic->arg[1] = (size_t) &cpu->cd.alpha.r[ra];
		ic->arg[0] = (iword & 0x001fffff) << 2;
		/*  Sign-extend:  */
		if (ic->arg[0] & 0x00400000)
			ic->arg[0] |= 0xffffffffff800000ULL;
		/*  Branches are calculated as PC + 4 + offset.  */
		ic->arg[0] = (size_t)(ic->arg[0] + 4);
		/*  Special case: branch within the same page:  */
		{
			uint64_t mask_within_page =
			    ((ALPHA_IC_ENTRIES_PER_PAGE-1) << 2) | 3;
			uint64_t old_pc = addr;
			uint64_t new_pc = old_pc + (int32_t)ic->arg[0];
			if ((old_pc & ~mask_within_page) ==
			    (new_pc & ~mask_within_page)) {
				ic->f = samepage_function;
				ic->arg[0] = (size_t) (
				    cpu->cd.alpha.cur_ic_page +
				    ((new_pc & mask_within_page) >> 2));
			}
		}
		break;
	default:if (!cpu->translation_readahead)
			fatal("[ UNIMPLEMENTED Alpha opcode 0x%x ]\n", opcode);
		goto bad;
	}


#define DYNTRANS_TO_BE_TRANSLATED_TAIL
#include "cpu_dyntrans.cc"
#undef	DYNTRANS_TO_BE_TRANSLATED_TAIL
}

