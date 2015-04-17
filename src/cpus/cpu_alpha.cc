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
 *  Alpha CPU emulation.
 *
 *  TODO: Many things.
 *
 *  See http://www.eecs.harvard.edu/~nr/toolkit/specs/alpha.html for info
 *  on instruction formats etc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "cpu.h"
#include "interrupt.h"
#include "machine.h"
#include "memory.h"
#include "misc.h"
#include "settings.h"
#include "symbol.h"

#define	DYNTRANS_8K
#define	DYNTRANS_PAGESIZE	8192
#include "tmp_alpha_head.cc"


/*  Alpha symbolic register names:  */
static const char *alpha_regname[N_ALPHA_REGS] = ALPHA_REG_NAMES; 

void alpha_irq_interrupt_assert(struct interrupt *interrupt);
void alpha_irq_interrupt_deassert(struct interrupt *interrupt);


/*
 *  alpha_cpu_new():
 *
 *  Create a new Alpha CPU object by filling the CPU struct.
 *  Return 1 on success, 0 if cpu_type_name isn't a valid Alpha processor.
 */
int alpha_cpu_new(struct cpu *cpu, struct memory *mem,
	struct machine *machine, int cpu_id, char *cpu_type_name)
{
	int i = 0;
	struct alpha_cpu_type_def cpu_type_defs[] = ALPHA_CPU_TYPE_DEFS;

	/*  Scan the cpu_type_defs list for this cpu type:  */
	while (cpu_type_defs[i].name != NULL) {
		if (strcasecmp(cpu_type_defs[i].name, cpu_type_name) == 0) {
			break;
		}
		i++;
	}
	if (cpu_type_defs[i].name == NULL)
		return 0;

	cpu->is_32bit = 0;
	cpu->byte_order = EMUL_LITTLE_ENDIAN;

	cpu->memory_rw = alpha_memory_rw;
	cpu->run_instr = alpha_run_instr;
	cpu->translate_v2p = alpha_translate_v2p;
	cpu->update_translation_table = alpha_update_translation_table;
	cpu->invalidate_translation_caches =
	    alpha_invalidate_translation_caches;
	cpu->invalidate_code_translation = alpha_invalidate_code_translation;

	cpu->cd.alpha.cpu_type = cpu_type_defs[i];

	/*  Only show name and caches etc for CPU nr 0:  */
	if (cpu_id == 0) {
		debug("%s", cpu->name);
	}

	cpu->cd.alpha.r[ALPHA_SP] = 0xfffffc000000ff00ULL;

	/*  Set up dummy kentry pointers to something which crashes
	    the machine:  */
	store_32bit_word(cpu, 0x10010, 0x3fffffc);
	for (i=0; i<N_ALPHA_KENTRY; i++)
		cpu->cd.alpha.kentry[i] = 0x10010;

	/*  Bogus initial context (will be overwritten on first
	    context switch):  */
	cpu->cd.alpha.ctx = 0x10100;

	CPU_SETTINGS_ADD_REGISTER64("pc", cpu->pc);
	for (i=0; i<N_ALPHA_REGS; i++)
		CPU_SETTINGS_ADD_REGISTER64(alpha_regname[i],
		    cpu->cd.alpha.r[i]);

	/*  Register the CPU interrupt pin:  */
	{
		struct interrupt templ;

		memset(&templ, 0, sizeof(templ));
		templ.line = 0;
		templ.name = cpu->path;
		templ.extra = cpu;
		templ.interrupt_assert = alpha_irq_interrupt_assert; 
		templ.interrupt_deassert = alpha_irq_interrupt_deassert; 
		interrupt_handler_register(&templ);
	}

	return 1;
}


/*
 *  alpha_cpu_dumpinfo():
 */
void alpha_cpu_dumpinfo(struct cpu *cpu)
{
	/*  TODO  */
	debug("\n");
}


/*
 *  alpha_cpu_list_available_types():
 *
 *  Print a list of available Alpha CPU types.
 */
void alpha_cpu_list_available_types(void)
{
	int i, j;
	struct alpha_cpu_type_def tdefs[] = ALPHA_CPU_TYPE_DEFS;

	i = 0;
	while (tdefs[i].name != NULL) {
		debug("%s", tdefs[i].name);
		for (j=13 - strlen(tdefs[i].name); j>0; j--)
			debug(" ");
		i++;
		if ((i % 4) == 0 || tdefs[i].name == NULL)
			debug("\n");
	}
}


/*
 *  alpha_cpu_register_dump():
 *  
 *  Dump cpu registers in a relatively readable format.
 *  
 *  gprs: set to non-zero to dump GPRs and some special-purpose registers.
 *  coprocs: set bit 0..3 to dump registers in coproc 0..3.
 */
void alpha_cpu_register_dump(struct cpu *cpu, int gprs, int coprocs)
{ 
	char *symbol;
	uint64_t offset;
	int i, x = cpu->cpu_id;

	if (gprs) {
		symbol = get_symbol_name(&cpu->machine->symbol_context,
		    cpu->pc, &offset);
		debug("cpu%i:\t pc = 0x%016"PRIx64, x, (uint64_t) cpu->pc);
		debug("  <%s>\n", symbol != NULL? symbol : " no symbol ");
		for (i=0; i<N_ALPHA_REGS; i++) {
			int r = (i >> 1) + ((i & 1) << 4);
			if ((i % 2) == 0)
				debug("cpu%i:\t", x);
			if (r != ALPHA_ZERO)
				debug("%3s = 0x%016"PRIx64, alpha_regname[r],
				    (uint64_t) cpu->cd.alpha.r[r]);
			if ((i % 2) == 1)
				debug("\n");
			else
				debug("   ");
		}
	}
}


/*
 *  alpha_cpu_tlbdump():
 *
 *  Called from the debugger to dump the TLB in a readable format.
 *  x is the cpu number to dump, or -1 to dump all CPUs.
 *
 *  If rawflag is nonzero, then the TLB contents isn't formated nicely,
 *  just dumped.
 */
void alpha_cpu_tlbdump(struct machine *m, int x, int rawflag)
{
}


/*
 *  alpha_irq_interrupt_assert():
 *  alpha_irq_interrupt_deassert():
 */
void alpha_irq_interrupt_assert(struct interrupt *interrupt)
{
	struct cpu *cpu = (struct cpu *) interrupt->extra;
	cpu->cd.alpha.irq_asserted = 1;
}
void alpha_irq_interrupt_deassert(struct interrupt *interrupt)
{
	struct cpu *cpu = (struct cpu *) interrupt->extra;
	cpu->cd.alpha.irq_asserted = 0;
}


/*
 *  alpha_print_imm16_disp():
 *
 *  Used internally by alpha_cpu_disassemble_instr().
 */
static void alpha_print_imm16_disp(int imm, int rb)
{
	imm = (int16_t)imm;

	if (imm < 0) {
		debug("-");
		imm = -imm;
	}
	if (imm <= 256)
		debug("%i", imm);
	else
		debug("0x%x", imm);
	if (rb != ALPHA_ZERO)
		debug("(%s)", alpha_regname[rb]);
}


/*
 *  alpha_cpu_disassemble_instr():
 *
 *  Convert an instruction word into human readable format, for instruction
 *  tracing.
 *              
 *  If running is 1, cpu->pc should be the address of the instruction.
 *
 *  If running is 0, things that depend on the runtime environment (eg.
 *  register contents) will not be shown, and addr will be used instead of
 *  cpu->pc for relative addresses.
 */                     
int alpha_cpu_disassemble_instr(struct cpu *cpu, unsigned char *ib,
        int running, uint64_t dumpaddr)
{
	uint32_t iw;
	uint64_t offset, tmp;
	int opcode, ra, rb, func, rc, imm, floating, rbrc = 0, indir = 0;
	const char *symbol, *mnem = NULL;
	char palcode_name[30];

	if (running)
		dumpaddr = cpu->pc;

	symbol = get_symbol_name(&cpu->machine->symbol_context,
	    dumpaddr, &offset);
	if (symbol != NULL && offset == 0)
		debug("<%s>\n", symbol);

	if (cpu->machine->ncpus > 1 && running)
		debug("cpu%i:\t", cpu->cpu_id);

	debug("%016"PRIx64":  ", (uint64_t) dumpaddr);

	iw = ib[0] + (ib[1]<<8) + (ib[2]<<16) + (ib[3]<<24);
	debug("%08x\t", (int)iw);

	opcode = iw >> 26;
	ra = (iw >> 21) & 31;
	rb = (iw >> 16) & 31;
	func = (iw >> 5) & 0x7ff;
	rc = iw & 31;
	imm = iw & 0xffff;

	switch (opcode) {
	case 0x00:
		alpha_palcode_name(iw & 0x3ffffff, palcode_name,
		    sizeof(palcode_name));
		debug("call_pal %s\n", palcode_name);
		break;
	case 0x08:
	case 0x09:
		debug("lda%s\t%s,", opcode == 9? "h" : "", alpha_regname[ra]);
		alpha_print_imm16_disp(imm, rb);
		debug("\n");
		break;
	case 0x0a:
	case 0x0b:
	case 0x0c:
	case 0x0d:
	case 0x0e:
	case 0x0f:
	case 0x20:
	case 0x21:
	case 0x22:
	case 0x23:
	case 0x24:
	case 0x25:
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
		floating = 0;
		switch (opcode) {
		case 0x0a: mnem = "ldbu"; break;
		case 0x0b: mnem = "ldq_u"; break;
		case 0x0c: mnem = "ldwu"; break;
		case 0x0d: mnem = "stw"; break;
		case 0x0e: mnem = "stb"; break;
		case 0x0f: mnem = "stq_u"; break;
		case 0x20: mnem = "ldf"; floating = 1; break;
		case 0x21: mnem = "ldg"; floating = 1; break;
		case 0x22: mnem = "lds"; floating = 1; break;
		case 0x23: mnem = "ldt"; floating = 1; break;
		case 0x24: mnem = "stf"; floating = 1; break;
		case 0x25: mnem = "stg"; floating = 1; break;
		case 0x26: mnem = "sts"; floating = 1; break;
		case 0x27: mnem = "stt"; floating = 1; break;
		case 0x28: mnem = "ldl"; break;
		case 0x29: mnem = "ldq"; break;
		case 0x2a: mnem = "ldl_l"; break;
		case 0x2b: mnem = "ldq_l"; break;
		case 0x2c: mnem = "stl"; break;
		case 0x2d: mnem = "stq"; break;
		case 0x2e: mnem = "stl_c"; break;
		case 0x2f: mnem = "stq_c"; break;
		}
		if (opcode == 0x0b && ra == ALPHA_ZERO) {
			debug("unop");
		} else {
			debug("%s\t", mnem);
			if (floating)
				debug("f%i,", ra);
			else
				debug("%s,", alpha_regname[ra]);
			alpha_print_imm16_disp(imm, rb);
		}
		debug("\n");
		break;
	case 0x10:
		switch (func & 0x7f) {
		case 0x00: mnem = "addl"; break;
		case 0x02: mnem = "s4addl"; break;
		case 0x09: mnem = "subl"; break;
		case 0x0b: mnem = "s4subl"; break;
		case 0x0f: mnem = "cmpbge"; break;
		case 0x12: mnem = "s8addl"; break;
		case 0x1b: mnem = "s8subl"; break;
		case 0x1d: mnem = "cmpult"; break;
		case 0x20: mnem = "addq"; break;
		case 0x22: mnem = "s4addq"; break;
		case 0x29: mnem = "subq"; break;
		case 0x2b: mnem = "s4subq"; break;
		case 0x2d: mnem = "cmpeq"; break;
		case 0x32: mnem = "s8addq"; break;
		case 0x3b: mnem = "s8subq"; break;
		case 0x3d: mnem = "cmpule"; break;
		case 0x40: mnem = "addl/v"; break;
		case 0x49: mnem = "subl/v"; break;
		case 0x4d: mnem = "cmplt"; break;
		case 0x60: mnem = "addq/v"; break;
		case 0x69: mnem = "subq/v"; break;
		case 0x6d: mnem = "cmple"; break;
		default:debug("UNIMPLEMENTED opcode 0x%x func 0x%x\n",
			    opcode, func);
		}
		if (mnem == NULL)
			break;
		if (func & 0x80)
			debug("%s\t%s,0x%x,%s\n", mnem,
			    alpha_regname[ra], (rb << 3) + (func >> 8),
			    alpha_regname[rc]);
		else
			debug("%s\t%s,%s,%s\n", mnem, alpha_regname[ra],
			    alpha_regname[rb], alpha_regname[rc]);
		break;
	case 0x11:
		switch (func & 0x7f) {
		case 0x000: mnem = "and"; break;
		case 0x008: mnem = "andnot"; break;
		case 0x014: mnem = "cmovlbs"; break;
		case 0x016: mnem = "cmovlbc"; break;
		case 0x020: mnem = "or"; break;
		case 0x024: mnem = "cmoveq"; break;
		case 0x026: mnem = "cmovne"; break;
		case 0x028: mnem = "ornot"; break;
		case 0x040: mnem = "xor"; break;
		case 0x044: mnem = "cmovlt"; break;
		case 0x046: mnem = "cmovge"; break;
		case 0x048: mnem = "eqv"; break;
		case 0x061: mnem = "amask"; break;
		case 0x064: mnem = "cmovle"; break;
		case 0x066: mnem = "cmovgt"; break;
		case 0x06c: mnem = "implver"; break;
		default:debug("UNIMPLEMENTED opcode 0x%x func 0x%x\n",
			    opcode, func);
		}
		if (mnem == NULL)
			break;
		/*  Special cases: "nop" etc:  */
		if (func == 0x020 && rc == ALPHA_ZERO)
			debug("nop\n");
		else if (func == 0x020 && (ra == ALPHA_ZERO
		    || rb == ALPHA_ZERO)) {
			if (ra == ALPHA_ZERO && rb == ALPHA_ZERO)
				debug("clr\t%s\n", alpha_regname[rc]);
			else if (ra == ALPHA_ZERO)
				debug("mov\t%s,%s\n", alpha_regname[rb],
				    alpha_regname[rc]);
			else
				debug("mov\t%s,%s\n", alpha_regname[ra],
				    alpha_regname[rc]);
		} else if (func == 0x1ec) {
			/*  implver  */
			debug("%s\t%s\n", mnem, alpha_regname[rc]);
		} else if (func & 0x80)
			debug("%s\t%s,0x%x,%s\n", mnem,
			    alpha_regname[ra], (rb << 3) + (func >> 8),
			    alpha_regname[rc]);
		else
			debug("%s\t%s,%s,%s\n", mnem, alpha_regname[ra],
			    alpha_regname[rb], alpha_regname[rc]);
		break;
	case 0x12:
		switch (func & 0x7f) {
		case 0x02: mnem = "mskbl"; break;
		case 0x06: mnem = "extbl"; break;
		case 0x0b: mnem = "insbl"; break;
		case 0x12: mnem = "mskwl"; break;
		case 0x16: mnem = "extwl"; break;
		case 0x1b: mnem = "inswl"; break;
		case 0x22: mnem = "mskll"; break;
		case 0x26: mnem = "extll"; break;
		case 0x2b: mnem = "insll"; break;
		case 0x30: mnem = "zap"; break;
		case 0x31: mnem = "zapnot"; break;
		case 0x32: mnem = "mskql"; break;
		case 0x34: mnem = "srl"; break;
		case 0x36: mnem = "extql"; break;
		case 0x39: mnem = "sll"; break;
		case 0x3b: mnem = "insql"; break;
		case 0x3c: mnem = "sra"; break;
		case 0x52: mnem = "mskwh"; break;
		case 0x57: mnem = "inswh"; break;
		case 0x5a: mnem = "extwh"; break;
		case 0x62: mnem = "msklh"; break;
		case 0x67: mnem = "inslh"; break;
		case 0x6a: mnem = "extlh"; break;
		case 0x72: mnem = "mskqh"; break;
		case 0x77: mnem = "insqh"; break;
		case 0x7a: mnem = "extqh"; break;
		default:debug("UNIMPLEMENTED opcode 0x%x func 0x%x\n",
			    opcode, func);
		}
		if (mnem == NULL)
			break;
		if (func & 0x80)
			debug("%s\t%s,0x%x,%s\n", mnem,
			    alpha_regname[ra], (rb << 3) + (func >> 8),
			    alpha_regname[rc]);
		else
			debug("%s\t%s,%s,%s\n", mnem, alpha_regname[ra],
			    alpha_regname[rb], alpha_regname[rc]);
		break;
	case 0x13:
		switch (func & 0x7f) {
		case 0x00: mnem = "mull"; break;
		case 0x20: mnem = "mulq"; break;
		case 0x30: mnem = "umulh"; break;
		case 0x40: mnem = "mull/v"; break;
		case 0x60: mnem = "mulq/v"; break;
		default:debug("UNIMPLEMENTED opcode 0x%x func 0x%x\n",
			    opcode, func);
		}
		if (mnem == NULL)
			break;
		if (func & 0x80)
			debug("%s\t%s,0x%x,%s\n", mnem,
			    alpha_regname[ra], (rb << 3) + (func >> 8),
			    alpha_regname[rc]);
		else
			debug("%s\t%s,%s,%s\n", mnem, alpha_regname[ra],
			    alpha_regname[rb], alpha_regname[rc]);
		break;
	case 0x16:
		switch (func & 0x7ff) {
		case 0x02f: mnem = "cvttq/c"; rbrc = 1; break;
		case 0x080: mnem = "adds"; break;
		case 0x081: mnem = "subs"; break;
		case 0x082: mnem = "muls"; break;
		case 0x083: mnem = "XXXx083"; break;
		case 0x0a0: mnem = "addt"; break;
		case 0x0a1: mnem = "subt"; break;
		case 0x0a2: mnem = "mult"; break;
		case 0x0a3: mnem = "divt"; break;
		case 0x0a5: mnem = "cmpteq"; break;
		case 0x0a6: mnem = "cmptlt"; break;
		case 0x0a7: mnem = "cmptle"; break;
		case 0x0be: mnem = "cvtqt"; rbrc = 1; break;
		default:debug("UNIMPLEMENTED opcode 0x%x func 0x%x\n",
			    opcode, func);
		}
		if (mnem == NULL)
			break;
		if (rbrc)
			debug("%s\tf%i,f%i\n", mnem, rb, rc);
		else
			debug("%s\tf%i,f%i,f%i\n", mnem, ra, rb, rc);
		break;
	case 0x17:
		switch (func & 0x7ff) {
		case 0x020: mnem = "fabs"; rbrc = 1; break;
		case 0x021: mnem = "fneg"; rbrc = 1; break;
		default:debug("UNIMPLEMENTED opcode 0x%x func 0x%x\n",
			    opcode, func);
		}
		if (mnem == NULL)
			break;
		if ((func & 0x7ff) == 0x020 && ra == 31 && rb == 31)
			debug("fclr\tf%i\n", rc);
		else if (rbrc)
			debug("%s\tf%i,f%i\n", mnem, rb, rc);
		else
			debug("%s\tf%i,f%i,f%i\n", mnem, ra, rb, rc);
		break;
	case 0x18:
		switch (iw & 0xffff) {
		case 0x0000: mnem = "trapb"; break;
		case 0x0400: mnem = "excb"; break;
		case 0x4000: mnem = "mb"; break;
		case 0x4400: mnem = "wmb"; break;
		case 0x8000: mnem = "fetch"; indir = 1; break;
		case 0xa000: mnem = "fetch_m"; indir = 1; break;
		case 0xc000: mnem = "rpcc"; break;
		case 0xe000: mnem = "rc"; break;
		case 0xe800: mnem = "ecb"; indir = 1; break;
		case 0xf000: mnem = "rs"; break;
		case 0xf800: mnem = "wh64"; indir = 1; break;
		default:debug("UNIMPLEMENTED opcode 0x%x func 0x%x\n",
			    opcode, func);
		}
		if (mnem == NULL)
			break;
		debug("%s", mnem);
		if ((iw & 0xffff) >= 0x8000) {
			debug("\t");
			if (indir)
				debug("(%s)", alpha_regname[rb]);
			else
				debug("%s", alpha_regname[ra]);
		}
		debug("\n");
		break;
	case 0x1a:
		tmp = iw & 0x3fff;
		if (tmp & 0x2000)
			tmp |= 0xffffffffffffc000ULL;
		tmp <<= 2;
		tmp += dumpaddr + sizeof(uint32_t);
		switch ((iw >> 14) & 3) {
		case 0:
		case 1:	if (((iw >> 14) & 3) == 0)
				debug("jmp");
			else
				debug("jsr");
			debug("\t%s,", alpha_regname[ra]);
			debug("(%s),", alpha_regname[rb]);
			debug("0x%"PRIx64, (uint64_t) tmp);
			symbol = get_symbol_name(&cpu->machine->symbol_context,
			    tmp, &offset);
			if (symbol != NULL)
				debug("\t<%s>", symbol);
			break;
		case 2:	debug("ret");
			break;
		default:fatal("unimpl JSR!");
		}
		debug("\n");
		break;
	case 0x30:
	case 0x34:
		tmp = iw & 0x1fffff;
		if (tmp & 0x100000)
			tmp |= 0xffffffffffe00000ULL;
		tmp <<= 2;
		tmp += dumpaddr + sizeof(uint32_t);
		debug("%s\t", opcode==0x30? "br" : "bsr");
		if (ra != ALPHA_ZERO)
			debug("%s,", alpha_regname[ra]);
		debug("0x%"PRIx64, (uint64_t) tmp);
		symbol = get_symbol_name(&cpu->machine->symbol_context,
		    tmp, &offset);
		if (symbol != NULL)
			debug("\t<%s>", symbol);
		debug("\n");
		break;
	case 0x31:
	case 0x35:
	case 0x38:
	case 0x39:
	case 0x3a:
	case 0x3b:
	case 0x3c:
	case 0x3d:
	case 0x3e:
	case 0x3f:
		floating = 0;
		switch (opcode) {
		case 0x31: mnem = "fbeq"; floating = 1; break;
		case 0x35: mnem = "fbne"; floating = 1; break;
		case 0x38: mnem = "blbc"; break;
		case 0x39: mnem = "beq"; break;
		case 0x3a: mnem = "blt"; break;
		case 0x3b: mnem = "ble"; break;
		case 0x3c: mnem = "blbs"; break;
		case 0x3d: mnem = "bne"; break;
		case 0x3e: mnem = "bge"; break;
		case 0x3f: mnem = "bgt"; break;
		}
		tmp = iw & 0x1fffff;
		if (tmp & 0x100000)
			tmp |= 0xffffffffffe00000ULL;
		tmp <<= 2;
		tmp += dumpaddr + sizeof(uint32_t);
		debug("%s\t", mnem);
		if (floating)
			debug("f%i,", ra);
		else
			debug("%s,", alpha_regname[ra]);
		debug("0x%"PRIx64, (uint64_t) tmp);
		symbol = get_symbol_name(&cpu->machine->symbol_context,
		    tmp, &offset);
		if (symbol != NULL)
			debug("\t<%s>", symbol);
		debug("\n");
		break;
	default:debug("UNIMPLEMENTED opcode 0x%x\n", opcode);
	}

	return sizeof(uint32_t);
}


#define MEMORY_RW       alpha_userland_memory_rw
#define MEM_ALPHA
#define MEM_USERLAND
#include "memory_rw.cc"
#undef MEM_USERLAND
#undef MEM_ALPHA
#undef MEMORY_RW


#include "tmp_alpha_tail.cc"

