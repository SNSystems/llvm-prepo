; RUN: not llvm-as < %s -o /dev/null 2>&1 | FileCheck %s
; Verify that repo.tickets is properly structured.
; repo.tickets takes a list of metadata entries.
; Each metadata entry can contain one ticket node only.

!repo.tickets = !{!0, !1}

!0 = !TicketNode(name: "factorial", digest: [16 x i8] c"+Th8\90\1D\9E/\A3\CF=\01\B3<v\DB", linkage: external)

!1 = !{i32 1}
; CHECK: assembly parsed, but does not verify as correct!
; CHECK-NEXT: invalid value for repo.tickets metadata entry operand (the operand should be a ticket node)
