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
        {ELFSectionType::BSS, {".bss", ELF::SHT_NOBITS, ELF::SHF_ALLOC | ELF::SHF_WRITE}},
        // X (Common)
        {ELFSectionType::Data, {".data", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_WRITE}},
        // X (RelRo)
        {ELFSectionType::Text, {".text", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_EXECINSTR}},
        {ELFSectionType::Mergeable1ByteCString,
         {".rodata.str1", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_MERGE | ELF::SHF_STRINGS}},
        {ELFSectionType::Mergeable1ByteCString,
         {".rodata.str2", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_MERGE | ELF::SHF_STRINGS}},
        {ELFSectionType::Mergeable1ByteCString,
         {".rodata.str4", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_MERGE | ELF::SHF_STRINGS}},
        {ELFSectionType::MergeableConst4,
         {".rodata.cst4", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_MERGE}},
        {ELFSectionType::MergeableConst8,
         {".rodata.cst8", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_MERGE}},
        {ELFSectionType::MergeableConst16,
         {".rodata.cst16", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_MERGE}},
        {ELFSectionType::MergeableConst32,
         {".rodata.cst32", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_MERGE}},
        {ELFSectionType::ReadOnly, {".rodata", ELF::SHT_PROGBITS, ELF::SHF_ALLOC}},
        {ELFSectionType::ThreadBSS,
         {".tbss", ELF::SHT_NOBITS, ELF::SHF_ALLOC | ELF::SHF_WRITE | ELF::SHF_TLS}},
        {ELFSectionType::ThreadData,
         {".tdata", ELF::SHT_PROGBITS, ELF::SHF_ALLOC | ELF::SHF_WRITE | ELF::SHF_TLS}},
        // X (Metadata)
        {ELFSectionType::InitArray,
         {".init_array", ELF::SHT_INIT_ARRAY, ELF::SHF_ALLOC | ELF::SHF_WRITE}},
        {ELFSectionType::FiniArray,
         {".fini_array", ELF::SHT_FINI_ARRAY, ELF::SHF_ALLOC | ELF::SHF_WRITE}},
    };

} // namespace details
