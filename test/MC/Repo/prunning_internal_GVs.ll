; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db llc -filetype=obj -debug-only repo-object %s -o %t 2>&1 | FileCheck %s

; REQUIRES: asserts

target triple = "x86_64-pc-linux-gnu-repo"

@a = internal global i32 1, align 4, !repo_ticket !0
@b = internal global i32 1, align 4, !repo_ticket !1

define internal i32 @foo() !repo_ticket !2 {
entry:
  ret i32 1
}

define internal i32 @bar() !repo_ticket !3 {
entry:
  ret i32 1
}

!repo.tickets = !{!2, !3, !0, !1}

!0 = !TicketNode(name: "a", digest: [16 x i8] c"\D0\AD(uUM\B749>\F5\01\0E\B2\D6[", linkage: internal, pruned: false)
!1 = !TicketNode(name: "b", digest: [16 x i8] c"\D0\AD(uUM\B749>\F5\01\0E\B2\D6[", linkage: internal, pruned: false)
!2 = !TicketNode(name: "foo", digest: [16 x i8] c"K\B6F\C6o\15\FD\E7se\FC\83\C8'\04+", linkage: internal, pruned: false)
!3 = !TicketNode(name: "bar", digest: [16 x i8] c"K\B6F\C6o\15\FD\E7se\FC\83\C8'\04+", linkage: internal, pruned: false)

;CHECK: A dummy section: section type 'text' and digest '2b0427c883fc6573e7fd156fc646b64b'
;CHECK: A dummy section: section type 'data' and digest '5bd6b20e01f53e3934b74d557528add0'
;CHECK: fragment 2b0427c883fc6573e7fd156fc646b64b adding.
;CHECK: fragment 5bd6b20e01f53e3934b74d557528add0 adding.
;CHECK: compilation member name 'foo' digest '2b0427c883fc6573e7fd156fc646b64b' adding.
;CHECK: compilation member name 'bar' digest '2b0427c883fc6573e7fd156fc646b64b' adding.
;CHECK: compilation member name 'a' digest '5bd6b20e01f53e3934b74d557528add0' adding.
;CHECK: compilation member name 'b' digest '5bd6b20e01f53e3934b74d557528add0' adding.
