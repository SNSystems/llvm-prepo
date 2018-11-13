; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S %s -o %t.ll
; RUN: env REPOFILE=%t.db llc -filetype=obj %t.ll -o %t.o
; RUN: env REPOFILE=%t.db repo2obj %t.o --repo  %t.db -o %t.elf
; RUN: llvm-readobj -sections -section-data  %t.elf | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

@a = global i32 0, align 4

;CHECK: Section {
;CHECK:   Name: .bss
;CHECK:   Type: SHT_NOBITS (0x8)
;CHECK:   Flags [ (0x3)
;CHECK:     SHF_ALLOC (0x2)
;CHECK:     SHF_WRITE (0x1)
;CHECK:   ]
;CHECK:   Address: 0x0
;CHECK:   Size: 4
;CHECK:   Link: 0
;CHECK:   Info: 0
;CHECK:   AddressAlignment: 4
;CHECK:   EntrySize: 0
;CHECK: }
