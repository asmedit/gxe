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

#include "StringHelper.h"


// This is basically strtoull(), but it needs to be explicitly implemented
// since some systems lack it. (Also, compiling with GNU C++ in ANSI mode
// does not work with strtoull.)
uint64_t StringHelper::ParseNumber(const char* str, bool& error)
{
	bool baseSet = false;
	int base = 10;
	uint64_t result = 0;
	bool negative = false;

	error = false;

	if (str == NULL)
		return 0;

	while (*str == ' ')
		++str;

	if (*str == '-') {
		negative = true;
		++str;
	}

	while ((*str == 'x' || *str == 'X') || (*str >= '0' && *str <= '9')
	    || (*str >= 'a' && *str <= 'f') || (*str >= 'A' && *str <= 'F')) {
		char c = *str;

		if (c == 'x' || c == 'X') {
			// Multiple base selections are not allowed.
			if (baseSet)
				break;

			// Only 0 prefix before base selection is allowed,
			// no other values.
			if (result != 0)
				break;

			base = 16;
			baseSet = true;
		} else {
			if (base == 10 && (c < '0' || c > '9'))
				break;

			int n = c - '0';
			if (c >= 'a' && c <= 'f')
				n = *str - 'a' + 10;
			if (c >= 'A' && c <= 'F')
				n = *str - 'A' + 10;

			if (base == 16 && (n < 0 || n > 15))
				break;

			result = result * base + n;
		}

		++str;
	}

	if (*str)
		error = true;

	if (negative)
		return -result;
	else
		return result;
}


vector<string> StringHelper::SplitStringIntoVector(const string &str, const char splitter)
{
	// This is slow and hackish, but works.
	vector<string> strings;
	string word;
	bool lastWasSplitter = false;

	for (size_t i=0, n=str.length(); i<n; i++) {
		char ch = str[i];
		if (ch == splitter) {
			strings.push_back(word);
			word = "";
			lastWasSplitter = true;
		} else {
			word += ch;
			lastWasSplitter = false;
		}
	}

	if (word != "" || lastWasSplitter)
		strings.push_back(word);

	return strings;
}


/*****************************************************************************/


#ifdef WITHUNITTESTS

static void Test_StringHelper_ParseNumber_Simple()
{
	string s = "42";
	bool error = true;

	uint64_t value = StringHelper::ParseNumber(s.c_str(), error);

	UnitTest::Assert("Should have succeeded with no error", error == false);
	UnitTest::Assert("Unexpected resulting value", value, 42);
}

static void Test_StringHelper_ParseNumber_SimpleError()
{
	string s = "Q42";
	bool error = false;

	StringHelper::ParseNumber(s.c_str(), error);

	UnitTest::Assert("Should have resulted in error", error == true);
}

static void Test_StringHelper_ParseNumber_Negative()
{
	string s = "-42";
	bool error = true;

	uint64_t value = StringHelper::ParseNumber(s.c_str(), error);

	UnitTest::Assert("Should have succeeded with no error", error == false);
	UnitTest::Assert("Unexpected resulting value", value, (uint64_t) -42);
}

static void Test_StringHelper_ParseNumber_LeadingSpaces()
{
	string s = "        42";
	bool error = true;

	uint64_t value = StringHelper::ParseNumber(s.c_str(), error);

	UnitTest::Assert("Should have succeeded with no error", error == false);
	UnitTest::Assert("Unexpected resulting value", value, 42);
}

static void Test_StringHelper_ParseNumber_LeadingSpacesAndNegative()
{
	string s = "        -42";
	bool error = true;

	uint64_t value = StringHelper::ParseNumber(s.c_str(), error);

	UnitTest::Assert("Should have succeeded with no error", error == false);
	UnitTest::Assert("Unexpected resulting value", value, (uint64_t) -42);
}

static void Test_StringHelper_ParseNumber_LeadingSpacesAndErrorNegative()
{
	string s = "        - 42";
	bool error = false;

	StringHelper::ParseNumber(s.c_str(), error);

	UnitTest::Assert("Should have resulted in error", error == true);
}

static void Test_StringHelper_ParseNumber_SimpleHexLowerCase()
{
	string s = "0x42a";
	bool error = true;

	uint64_t value = StringHelper::ParseNumber(s.c_str(), error);

	UnitTest::Assert("Should have succeeded with no error", error == false);
	UnitTest::Assert("Unexpected resulting value", value, 0x42a);
}

static void Test_StringHelper_ParseNumber_SimpleHexUpperCase()
{
	string s = "0X42A";
	bool error = true;

	uint64_t value = StringHelper::ParseNumber(s.c_str(), error);

	UnitTest::Assert("Should have succeeded with no error", error == false);
	UnitTest::Assert("Unexpected resulting value", value, 0x42a);
}

static void Test_StringHelper_ParseNumber_HexErrorDoubleX()
{
	string s = "0xx42";
	bool error = false;

	StringHelper::ParseNumber(s.c_str(), error);

	UnitTest::Assert("Should have resulted in error", error == true);
}

static void Test_StringHelper_ParseNumber_HexErrorNonZeroPrefix()
{
	string s = "04x42";
	bool error = false;

	StringHelper::ParseNumber(s.c_str(), error);

	UnitTest::Assert("Should have resulted in error", error == true);
}

static void Test_StringHelper_ParseNumber_NumberFollowedByErrorValidHexChar()
{
	string s = "42A";	// Note: A is a valid hex char
	bool error = false;

	StringHelper::ParseNumber(s.c_str(), error);

	UnitTest::Assert("Should have resulted in error", error == true);
}

static void Test_StringHelper_ParseNumber_NumberFollowedByError()
{
	string s = "42Q";
	bool error = false;

	StringHelper::ParseNumber(s.c_str(), error);

	UnitTest::Assert("Should have resulted in error", error == true);
}

static void Test_StringHelper_SplitStringIntoVector_Simple()
{
	vector<string> v = StringHelper::SplitStringIntoVector("A:B:C", ':');

	UnitTest::Assert("Wrong number of strings?", v.size(), 3);
	UnitTest::Assert("Wrong string contents?", v[0], "A");
	UnitTest::Assert("Wrong string contents?", v[1], "B");
	UnitTest::Assert("Wrong string contents?", v[2], "C");
}

static void Test_StringHelper_SplitStringIntoVector_EmptyInput()
{
	vector<string> v = StringHelper::SplitStringIntoVector("", ':');

	UnitTest::Assert("Wrong number of strings?", v.size(), 0);
}

static void Test_StringHelper_SplitStringIntoVector_Simple2()
{
	vector<string> v = StringHelper::SplitStringIntoVector("A:B:C", 'B');

	UnitTest::Assert("Wrong number of strings?", v.size(), 2);
	UnitTest::Assert("Wrong string contents?", v[0], "A:");
	UnitTest::Assert("Wrong string contents?", v[1], ":C");
}

static void Test_StringHelper_SplitStringIntoVector_WithZeroLengthParts()
{
	vector<string> v = StringHelper::SplitStringIntoVector("A::B:::C", ':');

	UnitTest::Assert("Wrong number of strings?", v.size(), 6);
	UnitTest::Assert("Wrong string contents?", v[0], "A");
	UnitTest::Assert("Wrong string contents?", v[1], "");
	UnitTest::Assert("Wrong string contents?", v[2], "B");
	UnitTest::Assert("Wrong string contents?", v[3], "");
	UnitTest::Assert("Wrong string contents?", v[4], "");
	UnitTest::Assert("Wrong string contents?", v[5], "C");
}

static void Test_StringHelper_SplitStringIntoVector_WithTrailingZeroLengthParts()
{
	vector<string> v = StringHelper::SplitStringIntoVector("A::", ':');

	UnitTest::Assert("Wrong number of strings?", v.size(), 3);
	UnitTest::Assert("Wrong string contents?", v[0], "A");
	UnitTest::Assert("Wrong string contents?", v[1], "");
	UnitTest::Assert("Wrong string contents?", v[2], "");
}

static void Test_StringHelper_SplitStringIntoVector_WithHeadingZeroLengthParts()
{
	vector<string> v = StringHelper::SplitStringIntoVector("A::", 'A');

	UnitTest::Assert("Wrong number of strings?", v.size(), 2);
	UnitTest::Assert("Wrong string contents?", v[0], "");
	UnitTest::Assert("Wrong string contents?", v[1], "::");
}

UNITTESTS(StringHelper)
{
	UNITTEST(Test_StringHelper_ParseNumber_Simple);
	UNITTEST(Test_StringHelper_ParseNumber_SimpleError);
	UNITTEST(Test_StringHelper_ParseNumber_Negative);
	UNITTEST(Test_StringHelper_ParseNumber_LeadingSpaces);
	UNITTEST(Test_StringHelper_ParseNumber_LeadingSpacesAndNegative);
	UNITTEST(Test_StringHelper_ParseNumber_LeadingSpacesAndErrorNegative);
	UNITTEST(Test_StringHelper_ParseNumber_SimpleHexLowerCase);
	UNITTEST(Test_StringHelper_ParseNumber_SimpleHexUpperCase);
	UNITTEST(Test_StringHelper_ParseNumber_HexErrorDoubleX);
	UNITTEST(Test_StringHelper_ParseNumber_HexErrorNonZeroPrefix);
	UNITTEST(Test_StringHelper_ParseNumber_NumberFollowedByErrorValidHexChar);
	UNITTEST(Test_StringHelper_ParseNumber_NumberFollowedByError);

	UNITTEST(Test_StringHelper_SplitStringIntoVector_Simple);
	UNITTEST(Test_StringHelper_SplitStringIntoVector_EmptyInput);
	UNITTEST(Test_StringHelper_SplitStringIntoVector_Simple2);
	UNITTEST(Test_StringHelper_SplitStringIntoVector_WithZeroLengthParts);
	UNITTEST(Test_StringHelper_SplitStringIntoVector_WithTrailingZeroLengthParts);
	UNITTEST(Test_StringHelper_SplitStringIntoVector_WithHeadingZeroLengthParts);
}

#endif

