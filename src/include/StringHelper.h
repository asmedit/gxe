#ifndef STRINGHELPER_H
#define	STRINGHELPER_H

/*
 *  Copyright (C) 2010  Anders Gavare.  All rights reserved.
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

#include "misc.h"

#include "UnitTest.h"


/**
 * \brief A helper class, with static functions for common string operations.
 */
class StringHelper
	: public UnitTestable
{
private:
	/**
	 * \brief No constructor.
	 */
	StringHelper();

public:
	/**
	 * \brief Parses a string into a 64-bit number.
	 *
	 * @param str A pointer to a character string.
	 * @param error Set to false if a parsed value is returned, true on
	 *	parse errors.
	 * @return A uint64_t, representing the parsed value.
	 */
	static uint64_t ParseNumber(const char* str, bool& error);

	/**
	 * \brief Splits a string with a certain delimiter into a vector of strings.
	 *
	 * E.g. if the input string is "A:B:C" and the splitter is ':', then
	 * the resulting vector consists of 3 strings: "A", "B", and "C".
	 *
	 * NOTE: The current implementation is very slow, but it at least it works.
	 *
	 * @param str A string to split.
	 * @param splitter Set to false if a parsed value is returned, true on
	 *	parse errors.
	 * @return A vector of strings (without the splitter char).
	 */
	static vector<string> SplitStringIntoVector(const string &str, const char splitter);

	/********************************************************************/

	static void RunUnitTests(int& nSucceeded, int& nFailures);
};


#endif	// STRINGHELPER_H
