; RUN: not llvm-as < %s 2>&1 | FileCheck %s

$v = comdat any
@v = global i32 0, comdat($v)
!0 = !TicketNode(name: "v", digest: [16 x i8] c"\DC\8BWeQ\E4\03\E6\F3:\DE\D1\9F\90\AC\F7", linkage: external, isComdat: 1)
; CHECK: expected 'true' or 'false'
