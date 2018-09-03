; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

@fp_foo = global [1 x i32 (...)*] [i32 (...)* bitcast (i32 ()* @foo to i32 (...)*)], align 8
@fp_bar = global [1 x i32 (...)*] [i32 (...)* bitcast (i32 ()* @bar to i32 (...)*)], align 8
@a = internal global i32 1, align 4
@vp_a = global [1 x i32*] [i32* @a], align 8
@b = internal global i32 1, align 4
@vp_b = global [1 x i32*] [i32* @b], align 8


define internal i32 @foo() {
entry:
  ret i32 1
}

define internal i32 @bar() {
entry:
  ret i32 1
}

;CHECK: !0 = !TicketNode(name: "fp_foo", digest: [16 x i8] c"\84WZ\86\0D?\A2\EB5\F1\97\ABh\0F\D6\04", linkage: external, pruned: false)
;CHECK: !1 = !TicketNode(name: "fp_bar", digest: [16 x i8] c"'hB\05u\14A\FA\C4\C7\B4\1C\9Fet\02", linkage: external, pruned: false)
;CHECK: !2 = !TicketNode(name: "a", digest: [16 x i8] c"\D0\AD(uUM\B749>\F5\01\0E\B2\D6[", linkage: internal, pruned: false)
;CHECK: !3 = !TicketNode(name: "vp_a", digest: [16 x i8] c"\0D\97\AD\B1\C6<\0C*\C3\CB\F6\EA\95\C3\0Fa", linkage: external, pruned: false)
;CHECK: !4 = !TicketNode(name: "b", digest: [16 x i8] c"\D0\AD(uUM\B749>\F5\01\0E\B2\D6[", linkage: internal, pruned: false)
;CHECK: !5 = !TicketNode(name: "vp_b", digest: [16 x i8] c"\C6\CEaX\C5~\CFI\B1\0Dd\94\91q\AA\9E", linkage: external, pruned: false)
;CHECK: !6 = !TicketNode(name: "foo", digest: [16 x i8] c"K\B6F\C6o\15\FD\E7se\FC\83\C8'\04+", linkage: internal, pruned: false)
;CHECK: !7 = !TicketNode(name: "bar", digest: [16 x i8] c"K\B6F\C6o\15\FD\E7se\FC\83\C8'\04+", linkage: internal, pruned: false)
