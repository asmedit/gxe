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
 *  Alpha ALU instructions.  (Included from tmp_alpha_misc.c.)
 *
 *
 *  Most ALU instructions have the following arguments:
 *  
 *  arg[0] = pointer to destination uint64_t
 *  arg[1] = pointer to source uint64_t nr 1
 *  arg[2] = pointer to source uint64_t nr 2
 *
 *  or, if ALU_IMM is set, arg[2] contains an 8-bit immediate value.
 *
 *  The main function groups are:
 *
 *	ALU_INS				inserts
 *	ALU_EXT				extracts
 *	ALU_MSK				masks
 *	ALU_CMOV			conditional moves
 *	ALU_CMP				compares
 *	ALU_CMPBGE			byte compare
 *	none of the above		everything else (add, sub, ...)
 */

void ALU_N(struct cpu *cpu, struct alpha_instr_call *ic)
{
#ifdef ALU_INS

	uint64_t x = *((uint64_t *)ic->arg[1]);
	int r = (
#ifdef ALU_IMM
	    ic->arg[2]
#else
	    (*((uint64_t *)ic->arg[2]))
#endif
	    & 7) * 8;

#ifdef ALU_B
	x &= 0xff;
#endif
#ifdef ALU_W
	x &= 0xffff;
#endif
#ifdef ALU_L
	x &= 0xffffffffULL;
#endif

#ifdef ALU_LO
	x <<= r;
#else
	r = 64 - r;
	if (r == 64)
		x = 0;
	else
		x >>= r;
#endif
	*((uint64_t *)ic->arg[0]) = x;

#else	/*  ! INS  */

#ifdef ALU_EXT

	uint64_t x = *((uint64_t *)ic->arg[1]);
	int r = (
#ifdef ALU_IMM
	    ic->arg[2]
#else
	    (*((uint64_t *)ic->arg[2]))
#endif
	    & 7) * 8;
#ifdef ALU_LO
	x >>= r;
#else
	r = 64 - r;
	if (r != 64)
		x <<= r;
#endif
#ifdef ALU_B
	x &= 0xff;
#endif
#ifdef ALU_W
	x &= 0xffff;
#endif
#ifdef ALU_L
	x &= 0xffffffffULL;
#endif
	*((uint64_t *)ic->arg[0]) = x;

#else	/*  ! EXT  */

#ifdef ALU_MSK

	uint64_t x = *((uint64_t *)ic->arg[1]);
#ifdef ALU_B
	uint64_t mask = 0x00000000000000ffULL;
#endif
#ifdef ALU_W
	uint64_t mask = 0x000000000000ffffULL;
#endif
#ifdef ALU_L
	uint64_t mask = 0x00000000ffffffffULL;
#endif
#ifdef ALU_Q
	uint64_t mask = 0xffffffffffffffffULL;
#endif
	int r = (
#ifdef ALU_IMM
	    ic->arg[2]
#else
	    (*((uint64_t *)ic->arg[2]))
#endif
	    & 7) * 8;

#ifdef ALU_LO
	mask <<= r;
#else
	if (r == 0)
		mask = 0;
	else
		mask >>= (64 - r);
#endif

	*((uint64_t *)ic->arg[0]) = x & ~mask;

#else	/*  !MSK  */

#ifdef ALU_CMOV

	if (
#ifdef ALU_CMOV_lbc
	    !(
#endif
	    (*((int64_t *)ic->arg[1]))
#ifdef ALU_CMOV_eq
	    == 0
#endif
#ifdef ALU_CMOV_ne
	    != 0
#endif
#ifdef ALU_CMOV_le
	    <= 0
#endif
#ifdef ALU_CMOV_lt
	    < 0
#endif
#ifdef ALU_CMOV_ge
	    >= 0
#endif
#ifdef ALU_CMOV_gt
	    > 0
#endif
#ifdef ALU_CMOV_lbs
	    & 1
#endif
#ifdef ALU_CMOV_lbc
	    & 1)
#endif
	    )
		*((uint64_t *)ic->arg[0]) =
#ifdef ALU_IMM
		    (uint64_t)ic->arg[2]
#else
		    (*((uint64_t *)ic->arg[2]))
#endif
		    ;

#else	/*  ! CMOV  */

#ifdef ALU_CMPBGE

	uint64_t ra = *((uint64_t *)ic->arg[1]), rc = 0, rb =
#ifdef ALU_IMM
		    (uint64_t)ic->arg[2]
#else
		    (*((uint64_t *)ic->arg[2]))
#endif
		    ;
	int i;
	for (i=7; i>=0; i--) {
		if ((uint8_t)ra >= (uint8_t)rb)
			rc |= (1 << i);
		rb >>= 8; ra >>= 8;
	}

	*((uint64_t *)ic->arg[0]) = rc;

#else	/*  ! CMPBGE  */

#ifdef ALU_CMP

	uint64_t x;

	x = (*((
#ifdef ALU_UNSIGNED
	    uint64_t
#else
	    int64_t
#endif
	    *)ic->arg[1]))

#ifdef ALU_CMP_EQ
	    ==
#endif
#ifdef ALU_CMP_LE
	    <=
#endif
#ifdef ALU_CMP_LT
	    <
#endif

#ifdef ALU_IMM
#ifdef ALU_UNSIGNED
	    (uint64_t)ic->arg[2]
#else
	    (int64_t)ic->arg[2]
#endif
#else
#ifdef ALU_UNSIGNED
	    (*((uint64_t *)ic->arg[2]))
#else
	    (*((int64_t *)ic->arg[2]))
#endif
#endif
	    ;

#else	/*  !ALU_CMP  */

#ifdef ALU_LONG
	/*  Long  */
	int32_t x;
#else
	/*  Quad  */
	int64_t x;
#endif

#ifdef ALU_ZAP
	/*  Prepare for zapping:  */
	uint64_t zapmask = 0xffffffffffffffffULL;
	int zapbytes =
#ifdef ALU_NOT
	    ~
#endif
#ifdef ALU_IMM
	    (int64_t)ic->arg[2]
#else
	    (*((uint64_t *)ic->arg[2]))
#endif
	    ;
	if (zapbytes & 0x80)
		zapmask &= ~0xff00000000000000ULL;
	if (zapbytes & 0x40)
		zapmask &= ~0xff000000000000ULL;
	if (zapbytes & 0x20)
		zapmask &= ~0xff0000000000ULL;
	if (zapbytes & 0x10)
		zapmask &= ~0xff00000000ULL;
	if (zapbytes & 0x08)
		zapmask &= ~0xff000000ULL;
	if (zapbytes & 0x04)
		zapmask &= ~0xff0000ULL;
	if (zapbytes & 0x02)
		zapmask &= ~0xff00ULL;
	if (zapbytes & 0x01)
		zapmask &= ~0xffULL;
#endif	/*  ZAP  */

	x = (
#ifdef ALU_SRA
	    (int64_t)
#endif
	    (*((uint64_t *)ic->arg[1]))
#ifdef ALU_S4
	    * 4
#endif
#ifdef ALU_S8
	    * 8
#endif
	    )
#ifdef ALU_ADD
	    +
#endif
#ifdef ALU_SUB
	    -
#endif
#ifdef ALU_OR
	    |
#endif
#ifdef ALU_XOR
	    ^
#endif
#ifdef ALU_AND
	    &
#endif
#ifdef ALU_SLL
	    <<
#endif
#if defined(ALU_SRA) || defined(ALU_SRL)
	    >>
#endif

#ifdef ALU_ZAP
	    & zapmask
#else	/*  !ZAP  */
	    (
#ifdef ALU_NOT
	    ~
#endif
	    (
#ifdef ALU_IMM
	    (int64_t)ic->arg[2]
#else
	    (*((uint64_t *)ic->arg[2]))
#endif
#if defined(ALU_SRA) || defined(ALU_SRL) || defined(ALU_SLL)
	    & 63
#endif
	    )
	    )
#endif	/*  !ZAP  */

	    ;

#endif	/*  !ALU_CMP  */

	*((uint64_t *)ic->arg[0]) = x;
#endif	/*  ! CMPBGE  */
#endif	/*  ! CMOV  */
#endif	/*  ! MSK  */
#endif	/*  ! EXT  */
#endif	/*  ! INS  */
}

