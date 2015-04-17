#ifndef CACHECOMPONENT_H
#define	CACHECOMPONENT_H

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

// COMPONENT(cache)


#include "AddressDataBus.h"
#include "MemoryMappedComponent.h"

#include "UnitTest.h"

#include <string.h>
#include <iomanip>


/**
 * \brief A memory Cache Component.
 *
 * TODO
 *
 * Note: This class does <i>not</i> handle unaligned access. It is up to the
 * caller to make sure that e.g. ReadData(uint64_t&, Endianness) is only
 * called when the selected address is 64-bit aligned.
 *
 * (The reason for this is that different emulated components want different
 * semantics for unaligned access. For example, an x86 processor will
 * transparently allow unaligned access, most RISC processors will cause
 * an unaligned address exception, and some old ARM processors may even simply
 * ignore the lowest bits of the address!)
 */
class CacheComponent
	: public Component
	, public AddressDataBus
	, public UnitTestable
{
public:
	/**
	 * \brief Constructs a CacheComponent.
	 *
	 * @param visibleClassName The visible class name. Defaults to
	 *	"cache". Useful alternatives may be "l1", "l2", or "l3".
	 */
	CacheComponent(const string& visibleClassName = "cache");

	virtual ~CacheComponent();

	/**
	 * \brief Creates a CacheComponent.
	 */
	static refcount_ptr<Component> Create(const ComponentCreateArgs& args);

	virtual void ResetState();

	string GenerateDetails() const;

	/**
	 * \brief Get attribute information about the CacheComponent class.
	 *
	 * @param attributeName The attribute name.
	 * @return A string representing the attribute value.
	 */
	static string GetAttribute(const string& attributeName);

        virtual void GetMethodNames(vector<string>& names) const;

	virtual bool MethodMayBeReexecutedWithoutArgs(const string& methodName) const;

	virtual void ExecuteMethod(GXemul* gxemul,
		const string& methodName,
		const vector<string>& arguments);

	/**
	 * \brief Returns the component's AddressDataBus interface.
	 *
	 * @return	A pointer to an AddressDataBus.
	 */
	virtual AddressDataBus* AsAddressDataBus();

	/* Implementation of AddressDataBus: */
	virtual void AddressSelect(uint64_t address);
	virtual bool ReadData(uint8_t& data, Endianness endianness);
	virtual bool ReadData(uint16_t& data, Endianness endianness);
	virtual bool ReadData(uint32_t& data, Endianness endianness);
	virtual bool ReadData(uint64_t& data, Endianness endianness);
	virtual bool WriteData(const uint8_t& data, Endianness endianness);
	virtual bool WriteData(const uint16_t& data, Endianness endianness);
	virtual bool WriteData(const uint32_t& data, Endianness endianness);
	virtual bool WriteData(const uint64_t& data, Endianness endianness);


	/********************************************************************/

	static void RunUnitTests(int& nSucceeded, int& nFailures);

private:

private:
	// State:
	uint64_t			m_size;		// total cache size
	uint64_t			m_lineSize;	// line size, in bytes
	uint64_t			m_lastDumpAddr;
	int				m_associativity;// 0 = fully. 1 = direct mapped. n = n-way.
	// TODO: the actual data (cache lines)

	// Cached/runtime state:
	uint64_t	m_addressSelect;  // For AddressDataBus read/write
};


#endif	// CACHECOMPONENT_H
