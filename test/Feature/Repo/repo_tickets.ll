; If the global variables or functions have been defined in the database 'clang.db', 
; the fragment is skipped and the ticket is added into database.
;
; The testcase includes two steps:
; Step 1: Create the database 'clang.db' which contains the 'sum' fragment.
; Step 2: run llc again to check that the ticket is added;
;
; RUN: rm clang.db
; RUN: llc -filetype=obj -debug-only repo-object %s -o %t 2>&1 | FileCheck %s

; REQUIRES: asserts

target triple = "x86_64-pc-linux-gnu-repo"

define i32 @sum(i32 %a, i32 %b) #0 !repo_ticket !0 {
entry:
  %a.addr = alloca i32, align 4
  %b.addr = alloca i32, align 4
  store i32 %a, i32* %a.addr, align 4
  store i32 %b, i32* %b.addr, align 4
  %0 = load i32, i32* %a.addr, align 4
  %1 = load i32, i32* %b.addr, align 4
  %add = add nsw i32 %0, %1
  ret i32 %add
}

!repo.tickets = !{!0}
!0 = !TicketNode(name: "sum", digest: [16 x i8] c"qd\BD6r\8A=\BB\05\8B\D8.\AA\BA\04P", linkage: external, pruned: false)

;CHECK: path: {{.*}}test{{/|\\}}Feature{{/|\\}}Repo{{/|\\}}Output
;CHECK: ticket name 'sum' digest '5004baaa2ed88b05bb3d8a7236bd6471' adding.

