; RUN: llc -filetype=obj -debug-only repo-object %s -o /dev/null 2>&1 | FileCheck %s

; REQUIRES: asserts

source_filename = "factorial.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu-repo"

; Function Attrs: noinline nounwind optnone
declare !fragment !0 i32 @factorial(i32) #0

; Function Attrs: noinline nounwind optnone
declare !fragment !1 i32 @fact3() #0

!repo.tickets = !{!0, !1}

!0 = !TicketNode(name: "factorial", digest: [16 x i8] c"+Th8\90\1D\9E/\A3\CF=\01\B3<v\DB", linkage: external)
!1 = !TicketNode(name: "fact3", digest: [16 x i8] c"\CA\FC.\06\8A\84\BE\14 \CB\7F5J\22l\19", linkage: external)

;CHECK: ticket name 'factorial' digest 'db763cb3013dcfa32f9e1d903868542b' adding.
;CHECK: ticket name 'fact3' digest '196c224a357fcb2014be848a062efcca' adding.
