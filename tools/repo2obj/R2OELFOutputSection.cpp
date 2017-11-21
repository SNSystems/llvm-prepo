//===-- R2OELFOutputSection.cpp -------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "R2OELFOutputSection.h"

using namespace llvm;
using namespace pstore::repo;

namespace details {

    SectionMap const SectionAttributes{
        {section_type::BSS, {".bss", ELF::SHT_NOBITS, ELF::SHF_ALLOC | ELF::SHF_WRITE}},
        // X (Common)
        {section_type::Data, {".data", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_WRITE}},
        // X (RelRo)
        {section_type::Text, {".text", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_EXECINSTR}},
        // X (Mergeable1ByteCString)
        // X (Mergeable2ByteCString)
        // X (Mergeable4ByteCString)
        // X (MergeableConst4)
        // X (MergeableConst8)
        // X (MergeableConst16)
        // X (MergeableConst32)
        // X (MergeableConst)
        {section_type::ReadOnly, {".rodata", ELF::SHT_PROGBITS, ELF::SHF_ALLOC}},
        {section_type::ThreadBSS,
         {".tbss", ELF::SHT_NOBITS, ELF::SHF_ALLOC | ELF::SHF_WRITE | ELF::SHF_TLS}},
        {section_type::ThreadData,
         {".tdata", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_WRITE | ELF::SHF_TLS}},
        // X (ThreadLocal)
        // X (Metadata)
    };

} // namespace details
