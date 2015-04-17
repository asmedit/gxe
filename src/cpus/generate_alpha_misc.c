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
 */

#include <stdio.h>
#include <string.h>


char *sizechar[4] = { "b", "w", "l", "q" };

#define N_CMPS	5
char *cmps[N_CMPS] = { "ult", "eq", "ule", "lt", "le" /*bge*/ };
#define N_CMOV	8
char *cmov[N_CMOV] = { "lbs", "lbc", "eq", "ne", "lt", "ge", "le", "gt" };


int main(int argc, char *argv[])
{
	int load, size, zero, n, msk, llsc;
	int ra, rc, lo, scale, imm, not, op, quad;

	printf("\n/*  AUTOMATICALLY GENERATED! Do not edit.  */\n\n");

	n = 0;
	/*  add and sub:  */
	for (imm = 0; imm <= 1; imm ++)
	  for (quad = 0; quad <= 1; quad ++)
	    for (scale = 0; scale <= 8; scale += 4)
		for (op = 0; op <= 1; op ++) {
			printf("#define ALU_N alpha_instr_");
			if (scale)
				printf("s%i", scale);
			printf("%s%s", op? "sub" : "add", quad? "q" : "l");
			if (imm)
				printf("_imm");
			printf("\n");

			if (imm)
				printf("#define ALU_IMM\n");
			if (!quad)
				printf("#define ALU_LONG\n");
			if (op)
				printf("#define ALU_SUB\n");
			else
				printf("#define ALU_ADD\n");
			if (scale)
				printf("#define ALU_S%i\n", scale);

			printf("#include \"cpu_alpha_instr_alu.cc\"\n");

			if (imm)
				printf("#undef ALU_IMM\n");
			if (!quad)
				printf("#undef ALU_LONG\n");
			if (op)
				printf("#undef ALU_SUB\n");
			else
				printf("#undef ALU_ADD\n");
			if (scale)
				printf("#undef ALU_S%i\n", scale);

			printf("#undef ALU_N\n");
		}

	/*  and, or, xor, zap, sll, srl, sra:  */
	for (imm = 0; imm <= 1; imm ++)
	    for (not = 0; not <= 1; not ++)
		for (op = 0; op < 7; op ++) {
			if (op >= 4 && not)
				continue;
			printf("#define ALU_N alpha_instr_");
			switch (op) {
			case 0: printf("and"); break;
			case 1: printf("or"); break;
			case 2: printf("xor"); break;
			case 3: printf("zap"); break;
			case 4: printf("sll"); break;
			case 5: printf("srl"); break;
			case 6: printf("sra"); break;
			}
			if (not)
				printf("not");
			if (imm)
				printf("_imm");
			printf("\n");
			if (imm)
				printf("#define ALU_IMM\n");
			switch (op) {
			case 0: printf("#define ALU_AND\n"); break;
			case 1: printf("#define ALU_OR\n"); break;
			case 2: printf("#define ALU_XOR\n"); break;
			case 3: printf("#define ALU_ZAP\n"); break;
			case 4: printf("#define ALU_SLL\n"); break;
			case 5: printf("#define ALU_SRL\n"); break;
			case 6: printf("#define ALU_SRA\n"); break;
			}
			if (not)
				printf("#define ALU_NOT\n");
			printf("#include \"cpu_alpha_instr_alu.cc\"\n");

			if (imm)
				printf("#undef ALU_IMM\n");
			if (not)
				printf("#undef ALU_NOT\n");
			switch (op) {
			case 0: printf("#undef ALU_AND\n"); break;
			case 1: printf("#undef ALU_OR\n"); break;
			case 2: printf("#undef ALU_XOR\n"); break;
			case 3: printf("#undef ALU_ZAP\n"); break;
			case 4: printf("#undef ALU_SLL\n"); break;
			case 5: printf("#undef ALU_SRL\n"); break;
			case 6: printf("#undef ALU_SRA\n"); break;
			}

			printf("#undef ALU_N\n");
		}

	printf("#define ALU_CMP\n");
	for (imm = 0; imm <= 1; imm ++)
	    for (op = 0; op < N_CMPS; op ++) {
		printf("#define ALU_N alpha_instr_cmp%s", cmps[op]);
		if (imm)
			printf("_imm");
		printf("\n");

		if (imm)
			printf("#define ALU_IMM\n");

		if (cmps[op][0] == 'u')
			printf("#define ALU_UNSIGNED\n");
		if (strcmp(cmps[op]+strlen(cmps[op])-2,"lt") == 0)
			printf("#define ALU_CMP_LT\n");
		if (strcmp(cmps[op]+strlen(cmps[op])-2,"le") == 0)
			printf("#define ALU_CMP_LE\n");
		if (strcmp(cmps[op]+strlen(cmps[op])-2,"eq") == 0)
			printf("#define ALU_CMP_EQ\n");

		printf("#include \"cpu_alpha_instr_alu.cc\"\n");

		if (cmps[op][0] == 'u')
			printf("#undef ALU_UNSIGNED\n");
		if (strcmp(cmps[op]+strlen(cmps[op])-2,"lt") == 0)
			printf("#undef ALU_CMP_LT\n");
		if (strcmp(cmps[op]+strlen(cmps[op])-2,"le") == 0)
			printf("#undef ALU_CMP_LE\n");
		if (strcmp(cmps[op]+strlen(cmps[op])-2,"eq") == 0)
			printf("#undef ALU_CMP_EQ\n");
		if (imm)
			printf("#undef ALU_IMM\n");
		printf("#undef ALU_N\n");
	    }
	printf("#undef ALU_CMP\n");

	printf("#define ALU_CMOV\n");
	for (imm = 0; imm <= 1; imm ++)
	    for (op = 0; op < N_CMOV; op ++) {
		printf("#define ALU_N alpha_instr_cmov%s", cmov[op]);
		if (imm)
			printf("_imm");
		printf("\n");
		if (imm)
			printf("#define ALU_IMM\n");
		printf("#define ALU_CMOV_%s\n", cmov[op]);
		printf("#include \"cpu_alpha_instr_alu.cc\"\n");
		printf("#undef ALU_CMOV_%s\n", cmov[op]);
		if (imm)
			printf("#undef ALU_IMM\n");
		printf("#undef ALU_N\n");
	    }
	printf("#undef ALU_CMOV\n");

	printf("#define ALU_CMPBGE\n");
	for (imm = 0; imm <= 1; imm ++) {
		printf("#define ALU_N alpha_instr_cmpbge");
		if (imm)
			printf("_imm");
		printf("\n");
		if (imm)
			printf("#define ALU_IMM\n");
		printf("#include \"cpu_alpha_instr_alu.cc\"\n");
		if (imm)
			printf("#undef ALU_IMM\n");
		printf("#undef ALU_N\n");
	    }
	printf("#undef ALU_CMPBGE\n");

	for (imm = 0; imm <= 1; imm ++)
	  for (lo = 0; lo <= 1; lo ++)
	   for (msk = 0; msk <= 2; msk ++)
	    for (size=0; size<4; size++) {
		if (size==0 && lo==0)
			continue;
		switch (msk) {
		case 0:	printf("#define ALU_MSK\n"); break;
		case 1:	printf("#define ALU_EXT\n"); break;
		case 2:	printf("#define ALU_INS\n"); break;
		}
		switch (msk) {
		case 0:	printf("#define ALU_N alpha_instr_msk"); break;
		case 1:	printf("#define ALU_N alpha_instr_ext"); break;
		case 2:	printf("#define ALU_N alpha_instr_ins"); break;
		}
		printf("%s", sizechar[size]);
		if (lo)
			printf("l");
		else
			printf("h");
		if (imm)
			printf("_imm");
		printf("\n");
		if (imm)
			printf("#define ALU_IMM\n");
		switch (size) {
		case 0:	printf("#define ALU_B\n"); break;
		case 1:	printf("#define ALU_W\n"); break;
		case 2:	printf("#define ALU_L\n"); break;
		case 3:	printf("#define ALU_Q\n"); break;
		}
		if (lo)
			printf("#define ALU_LO\n");
		printf("#include \"cpu_alpha_instr_alu.cc\"\n");
		switch (size) {
		case 0:	printf("#undef ALU_B\n"); break;
		case 1:	printf("#undef ALU_W\n"); break;
		case 2:	printf("#undef ALU_L\n"); break;
		case 3:	printf("#undef ALU_Q\n"); break;
		}
		switch (msk) {
		case 0:	printf("#undef ALU_MSK\n"); break;
		case 1:	printf("#undef ALU_EXT\n"); break;
		case 2:	printf("#undef ALU_INS\n"); break;
		}
		if (lo)
			printf("#undef ALU_LO\n");
		if (imm)
			printf("#undef ALU_IMM\n");
		printf("#undef ALU_N\n");
	    }

	/*
	 *  Normal load/store:
	 */
	for (llsc=0; llsc<=1; llsc++)
	    for (load=0; load<=1; load++)
		for (zero=0; zero<=1; zero++)
		    for (size=0; size<4; size++) {
			if (llsc && size < 2)
				continue;
			if (zero)
				printf("#define LS_IGNORE_OFFSET\n");
			if (load)
				printf("#define LS_LOAD\n");
			if (llsc)
				printf("#define LS_LLSC\n");
			switch (size) {
			case 0:	printf("#define LS_B\n"); break;
			case 1:	printf("#define LS_W\n"); break;
			case 2:	printf("#define LS_L\n"); break;
			case 3:	printf("#define LS_Q\n"); break;
			}
			printf("#define LS_GENERIC_N alpha_generic_");
			if (load)
				printf("ld");
			else
				printf("st");
			printf("%s", sizechar[size]);
			if (llsc)
				printf("_llsc");
			printf("\n");
			printf("#define LS_N alpha_instr_");
			if (load)
				printf("ld");
			else
				printf("st");
			printf("%s", sizechar[size]);
			if (zero)
				printf("_0");
			if (llsc)
				printf("_llsc");
			printf("\n");
			printf("#include \"cpu_alpha_instr_loadstore.cc\"\n");
			printf("#undef LS_N\n");
			printf("#undef LS_GENERIC_N\n");
			switch (size) {
			case 0:	printf("#undef LS_B\n"); break;
			case 1:	printf("#undef LS_W\n"); break;
			case 2:	printf("#undef LS_L\n"); break;
			case 3:	printf("#undef LS_Q\n"); break;
			}
			if (load)
				printf("#undef LS_LOAD\n");
			if (llsc)
				printf("#undef LS_LLSC\n");
			if (zero)
				printf("#undef LS_IGNORE_OFFSET\n");
		    }

	/*
	 *  Unaligned load/store:
	 */
	printf("#define LS_UNALIGNED\n");
	for (load=0; load<=1; load++) {
			size = 3;
			if (load)
				printf("#define LS_LOAD\n");
			printf("#define LS_Q\n");
			printf("#define LS_GENERIC_N alpha_generic_");
			if (load)
				printf("ld");
			else
				printf("st");
			printf("%s", sizechar[size]);
			printf("_u");		/*  NOTE: unaligned  */
			printf("\n");
			printf("#define LS_N alpha_instr_");
			if (load)
				printf("ld");
			else
				printf("st");
			printf("%s", sizechar[size]);
			printf("_u");		/*  NOTE: unaligned  */
			printf("\n");
			printf("#include \"cpu_alpha_instr_loadstore.cc\"\n");
			printf("#undef LS_N\n");
			printf("#undef LS_GENERIC_N\n");
			printf("#undef LS_Q\n");
			if (load)
				printf("#undef LS_LOAD\n");
		}
	printf("#undef LS_UNALIGNED\n");

	/*  Lookup table for most normal loads/stores:  */
	printf("\n\nvoid (*alpha_loadstore[32])(struct cpu *, struct "
	    "alpha_instr_call *) = {\n");

	for (llsc = 0; llsc <= 1; llsc ++)
	    for (load=0; load<=1; load++)
		for (zero=0; zero<=1; zero++)
		    for (size=0; size<4; size++) {
			printf("\talpha_instr_");
			if (llsc && (size != 2 && size != 3)) {
				printf("nop");
			} else {
				if (load)
					printf("ld");
				else
					printf("st");
				printf("%s", sizechar[size]);
				if (zero)
					printf("_0");
				if (llsc)
					printf("_llsc");
			}
			if (++n < 64)
				printf(",");
			printf("\n");
		    }

	printf("};\n\n");

	for (ra = 0; ra < 32; ra ++)
	    for (rc = 0; rc < 31; rc ++)
		if (ra != rc) {
			printf("static void alpha_instr_mov_%i_%i(struct cpu"
			    " *cpu, struct alpha_instr_call *ic)\n", ra, rc);
			printf("{ cpu->cd.alpha.r[%i] = ", rc);
			if (ra == 31)
				printf("0");
			else
				printf("cpu->cd.alpha.r[%i]", ra);
			printf("; }\n");
		}

	printf("\n\nvoid (*alpha_mov_r_r[32*31])(struct cpu *, struct "
	    "alpha_instr_call *) = {\n");
	n = 0;
	for (rc = 0; rc < 31; rc ++)
	    for (ra = 0; ra < 32; ra ++) {
		if (ra == rc)
			printf("\talpha_instr_nop");
		else
			printf("\talpha_instr_mov_%i_%i", ra, rc);
		if (++n < 31*32)
			printf(",");
		printf("\n");
	    }

	printf("};\n\n");

	return 0;
}

