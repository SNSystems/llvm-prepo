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

namespace details {

    SectionMap const SectionAttributes{
        // X (BSS)
        // X (Common)
        // X (Data)
        {pstore::repo::section_type::Data,
         {".data", ELF::SHT_PROGBITS, (unsigned) (ELF::SHF_ALLOC | ELF::SHF_WRITE)}},
        // X (RelRo)
        {pstore::repo::section_type::Text,
         {".text", ELF::SHT_PROGBITS, (unsigned) ELF::SHF_ALLOC | ELF::SHF_EXECINSTR}},
        // X (Mergeable1ByteCString)
        // X (Mergeable2ByteCString)
        // X (Mergeable4ByteCString)
        // X (MergeableConst4)
        // X (MergeableConst8)
        // X (MergeableConst16)
        // X (MergeableConst32)
        // X (MergeableConst)
        {pstore::repo::section_type::ReadOnly, {".rodata", ELF::SHT_PROGBITS, ELF::SHF_ALLOC}},
        // X (ThreadBSS)
        // X (ThreadData)
        // X (ThreadLocal)
        // X (Metadata)
    };

} // namespace details
