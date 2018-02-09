; Check Repo supports common symbols.
;
; RUN: env REPOFILE=%T/clang.db llc -filetype=obj %s -o %t
; RUN: repo2obj %t --repo %T/clang.db -o %t1
; RUN: llvm-readobj -t %t1 | FileCheck %s


target triple = "x86_64-pc-linux-gnu-repo"

@a = common global i32 0, align 4, !repo_ticket !0

!repo.tickets = !{!0}

!0 = !TicketNode(name: "a", digest: [16 x i8] c"\22\CE\E5\A0\D2t\C9h\9D\D1M\15\F7L\B4\A2", linkage: common)

; CHECK: Name: a (
; CHECK-NEXT: Value: 0x0
; CHECK-NEXT: Size: 4
; CHECK-NEXT: Binding: Global (0x1)
; CHECK-NEXT: Type: Object (0x1)
; CHECK-NEXT: Other: 0
; CHECK-NEXT: Section: Common (0xFFF2)
