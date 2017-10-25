; RUN: not llc -filetype=obj < %s 2>&1 | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

; Function Attrs: noinline nounwind optnone
declare !fragment !0 i32 @factorial(i32) #0

!0 = !TicketNode(name: "factorial", digest: [16 x i8] c"+Th8\90\1D\9E/\A3\CF=\01\B3<v\DB", linkage: external, isComdat: false)

; CHECK: Failed to get 'repo.tickets' module metadata!
