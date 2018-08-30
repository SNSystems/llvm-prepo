; RUN: llc -filetype=obj %S/repo_common.ll -o /dev/null 2>&1
; RUN: llc -filetype=obj -debug-only repo-object %s -o /dev/null 2>&1 | FileCheck %s

; REQUIRES: asserts

source_filename = "test.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu-repo"

@.str = external unnamed_addr constant [13 x i8] , align 1, !repo_ticket !0
@.str.1 = private unnamed_addr constant [13 x i8] c"Hello World\0A\00", align 1, !repo_ticket !1


define i32 @main() !repo_ticket !2 {
entry:
  %call = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str, i32 0, i32 0))
  %call1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.1, i32 0, i32 0))
  ret i32 0
}

declare i32 @printf(i8*, ...)

!repo.tickets = !{!0, !1, !2}

!0 = !TicketNode(name: ".str", digest: [16 x i8] c"\FE\15n\B4!\B1\FC{(\E5^>\E5\E2\F5\00", linkage: private)
!1 = !TicketNode(name: ".str.1", digest: [16 x i8] c"\FE\15n\B4!\B1\FC{(\E5^>\E5\E2\F5\00", linkage: private)
!2 = !TicketNode(name: "main", digest: [16 x i8] c"|!wp\FB\AE\8F\D0\CE\D3\1AO\E4\17\A3\C2", linkage: external)

;CHECK: insert name: .str
;CHECK: insert name: .str.1
;CHECK-NOT: insert name: .L.str
;CHECK-NOT: insert name: .L.str.1
