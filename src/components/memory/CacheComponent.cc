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

#include "components/CacheComponent.h"
#include "GXemul.h"


CacheComponent::CacheComponent(const string& visibleClassName)
	: Component("cache", visibleClassName)
	, m_size(0)
	, m_lineSize(64)
	, m_lastDumpAddr(0)
	, m_associativity(1)
	, m_addressSelect(0)
{
	AddVariable("size", &m_size);
	AddVariable("lineSize", &m_lineSize);
	AddVariable("lastDumpAddr", &m_lastDumpAddr);
	AddVariable("associativity", &m_associativity);
//	AddCustomVariable("data", &m_dataHandler);
}


CacheComponent::~CacheComponent()
{
}


refcount_ptr<Component> CacheComponent::Create(const ComponentCreateArgs& args)
{
	return new CacheComponent();
}


string CacheComponent::GetAttribute(const string& attributeName)
{
	if (attributeName == "stable")
		return "yes";

	if (attributeName == "description")
		return "A generic memory cache component.";

	return Component::GetAttribute(attributeName);
}


string CacheComponent::GenerateDetails() const
{
	stringstream ss;
	ss << Component::GenerateDetails();

	if (!ss.str().empty())
		ss << ", ";

	if (m_size >= (1 << 30))
		ss << (m_size >> 30) << " GB";
	else if (m_size >= (1 << 20))
		ss << (m_size >> 20) << " MB";
	else if (m_size >= (1 << 10))
		ss << (m_size >> 10) << " KB";
	else if (m_size != 1)
		ss << m_size << " bytes";
	else
		ss << m_size << " byte";

	if (m_associativity == 0)
		ss << ", fully associative, ";
	else if (m_associativity == 1)
		ss << ", direct-mapped, ";
	else
		ss << ", " << m_associativity << "-way, ";

	if (m_lineSize >= (1 << 30))
		ss << (m_lineSize >> 30) << " GB";
	else if (m_lineSize >= (1 << 20))
		ss << (m_lineSize >> 20) << " MB";
	else if (m_lineSize >= (1 << 10))
		ss << (m_lineSize >> 10) << " KB";
	else if (m_lineSize != 1)
		ss << m_lineSize << " bytes";
	else
		ss << m_lineSize << " byte";

	ss << " per line";

	return ss.str();
}


void CacheComponent::ResetState()
{
}


void CacheComponent::GetMethodNames(vector<string>& names) const
{
	// Add our method names...
	names.push_back("dump");

	// ... and make sure to call the base class implementation:
	Component::GetMethodNames(names);
}


bool CacheComponent::MethodMayBeReexecutedWithoutArgs(const string& methodName) const
{
	if (methodName == "dump")
		return true;

	// ... and make sure to call the base class implementation:
	return Component::MethodMayBeReexecutedWithoutArgs(methodName);
}


void CacheComponent::ExecuteMethod(GXemul* gxemul, const string& methodName,
	const vector<string>& arguments)
{
	if (methodName == "dump") {
		uint64_t vaddr = m_lastDumpAddr;

		if (arguments.size() > 1) {
			gxemul->GetUI()->ShowDebugMessage("syntax: .dump [addr]\n");
			return;
		}

		if (arguments.size() == 1) {
			gxemul->GetUI()->ShowDebugMessage("TODO: parse address expression\n");
			gxemul->GetUI()->ShowDebugMessage("(for now, only hex immediate values are supported!)\n");

			stringstream ss;
			ss << arguments[0];
			ss.flags(std::ios::hex);
			ss >> vaddr;
		}

		const int nRows = 16;
		for (int i=0; i<nRows; i++) {
			const size_t len = 16;
			unsigned char data[len];
			bool readable[len];

			stringstream ss;
			ss.flags(std::ios::hex);

			if (vaddr > 0xffffffff)
				ss << std::setw(16);
			else
				ss << std::setw(8);

			ss << std::setfill('0') << vaddr;

			size_t k;
			for (k=0; k<len; ++k) {
				AddressSelect(vaddr + k);
				readable[k] = ReadData(data[k], BigEndian);
			}
			
			ss << " ";
			
			for (k=0; k<len; ++k) {
				if ((k&3) == 0)
					ss << " ";

				ss << std::setw(2) << std::setfill('0');
				if (readable[k])
					ss << (int)data[k];
				else
					ss << "--";
			}

			ss << "  ";

			for (k=0; k<len; ++k) {
				char s[2];
				s[0] = data[k] >= 32 && data[k] < 127? data[k] : '.';
				s[1] = '\0';
				
				if (readable[k])
					ss << s;
				else
					ss << "-";
			}
			
			ss << "\n";

			gxemul->GetUI()->ShowDebugMessage(ss.str());

			vaddr += len;
		}

		m_lastDumpAddr = vaddr;

		return;
	}

	// Call base...
	Component::ExecuteMethod(gxemul, methodName, arguments);
}


AddressDataBus* CacheComponent::AsAddressDataBus()
{
	return this;
}


void CacheComponent::AddressSelect(uint64_t address)
{
#if 0
	m_addressSelect = address;

	uint64_t blockNr = address >> m_blockSizeShift;

	if (blockNr+1 > m_memoryBlocks.size())
		m_selectedHostMemoryBlock = NULL;
	else
		m_selectedHostMemoryBlock = m_memoryBlocks[blockNr];

	m_selectedOffsetWithinBlock = address & (m_blockSize-1);
#endif
}


bool CacheComponent::ReadData(uint8_t& data, Endianness endianness)
{
#if 0
	if (m_selectedHostMemoryBlock == NULL)
		data = 0;
	else
		data = (((uint8_t*)m_selectedHostMemoryBlock)
		    [m_selectedOffsetWithinBlock]);

	return true;
#endif
	return false;
}


bool CacheComponent::ReadData(uint16_t& data, Endianness endianness)
{
#if 0
	assert((m_addressSelect & 1) == 0);

	if (m_selectedHostMemoryBlock == NULL)
		data = 0;
	else
		data = (((uint16_t*)m_selectedHostMemoryBlock)
		    [m_selectedOffsetWithinBlock >> 1]);

	if (endianness == BigEndian)
		data = BE16_TO_HOST(data);
	else
		data = LE16_TO_HOST(data);

	return true;
#endif
	return false;
}


bool CacheComponent::ReadData(uint32_t& data, Endianness endianness)
{
#if 0
	assert((m_addressSelect & 3) == 0);

	if (m_selectedHostMemoryBlock == NULL)
		data = 0;
	else
		data = (((uint32_t*)m_selectedHostMemoryBlock)
		    [m_selectedOffsetWithinBlock >> 2]);

	if (endianness == BigEndian)
		data = BE32_TO_HOST(data);
	else
		data = LE32_TO_HOST(data);

	return true;
#endif
	return false;
}


bool CacheComponent::ReadData(uint64_t& data, Endianness endianness)
{
#if 0
	assert((m_addressSelect & 7) == 0);

	if (m_selectedHostMemoryBlock == NULL)
		data = 0;
	else
		data = (((uint64_t*)m_selectedHostMemoryBlock)
		    [m_selectedOffsetWithinBlock >> 3]);

	if (endianness == BigEndian)
		data = BE64_TO_HOST(data);
	else
		data = LE64_TO_HOST(data);

	return true;
#endif
	return false;
}


bool CacheComponent::WriteData(const uint8_t& data, Endianness endianness)
{
#if 0
	if (m_writeProtected)
		return false;

	if (m_selectedHostMemoryBlock == NULL)
		m_selectedHostMemoryBlock = AllocateBlock();

	(((uint8_t*)m_selectedHostMemoryBlock)
	    [m_selectedOffsetWithinBlock]) = data;

	return true;
#endif
	return false;
}


bool CacheComponent::WriteData(const uint16_t& data, Endianness endianness)
{
#if 0
	assert((m_addressSelect & 1) == 0);

	if (m_writeProtected)
		return false;

	if (m_selectedHostMemoryBlock == NULL)
		m_selectedHostMemoryBlock = AllocateBlock();

	uint16_t d;
	if (endianness == BigEndian)
		d = BE16_TO_HOST(data);
	else
		d = LE16_TO_HOST(data);

	(((uint16_t*)m_selectedHostMemoryBlock)
	    [m_selectedOffsetWithinBlock >> 1]) = d;

	return true;
#endif
	return false;
}


bool CacheComponent::WriteData(const uint32_t& data, Endianness endianness)
{
#if 0
	assert((m_addressSelect & 3) == 0);

	if (m_writeProtected)
		return false;

	if (m_selectedHostMemoryBlock == NULL)
		m_selectedHostMemoryBlock = AllocateBlock();

	uint32_t d;
	if (endianness == BigEndian)
		d = BE32_TO_HOST(data);
	else
		d = LE32_TO_HOST(data);

	(((uint32_t*)m_selectedHostMemoryBlock)
	    [m_selectedOffsetWithinBlock >> 2]) = d;

	return true;
#endif
	return false;
}


bool CacheComponent::WriteData(const uint64_t& data, Endianness endianness)
{
#if 0
	assert((m_addressSelect & 7) == 0);

	if (m_writeProtected)
		return false;

	if (m_selectedHostMemoryBlock == NULL)
		m_selectedHostMemoryBlock = AllocateBlock();

	uint64_t d;
	if (endianness == BigEndian)
		d = BE64_TO_HOST(data);
	else
		d = LE64_TO_HOST(data);

	(((uint64_t*)m_selectedHostMemoryBlock)
	    [m_selectedOffsetWithinBlock >> 3]) = d;

	return true;
#endif
	return false;
}


/*****************************************************************************/


#ifdef WITHUNITTESTS

#include "ComponentFactory.h"

static void Test_CacheComponent_IsStable()
{
	UnitTest::Assert("the CacheComponent should be stable",
	    ComponentFactory::HasAttribute("cache", "stable"));
}

static void Test_CacheComponent_AddressDataBus()
{
	refcount_ptr<Component> ram = ComponentFactory::CreateComponent("cache");

	AddressDataBus* bus = ram->AsAddressDataBus();
	UnitTest::Assert("The CacheComponent should implement the "
	    "AddressDataBus interface", bus != NULL);
}

UNITTESTS(CacheComponent)
{
	UNITTEST(Test_CacheComponent_IsStable);
	UNITTEST(Test_CacheComponent_AddressDataBus);
}

#endif

