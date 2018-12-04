; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

@fp_foo = global [1 x i32 (...)*] [i32 (...)* bitcast (i32 ()* @foo to i32 (...)*)], align 8
@fp_bar = global [1 x i32 (...)*] [i32 (...)* bitcast (i32 ()* @bar to i32 (...)*)], align 8
@a = internal global i32 1, align 4
@vp_a = global [1 x i32*] [i32* @a], align 8
@b = internal global i32 2, align 4
@vp_b = global [1 x i32*] [i32* @b], align 8

define internal i32 @foo() {
entry:
  ret i32 1
}

define internal i32 @bar() {
entry:
  ret i32 2
}

;CHECK: !0 = !TicketNode(name: "fp_foo", digest: [16 x i8] c"\91\16\81\CD\15\C0 Bd*\13\E2\12\92-\E7", linkage: external, pruned: false)
;CHECK: !1 = !TicketNode(name: "fp_bar", digest: [16 x i8] c"\B1\B1c\C0\06c+\C6\E6}\8A\15\DBz%3", linkage: external, pruned: false)
;CHECK: !2 = !TicketNode(name: "a", digest: [16 x i8] c"\D0\AD(uUM\B749>\F5\01\0E\B2\D6[", linkage: internal, pruned: false)
;CHECK: !3 = !TicketNode(name: "vp_a", digest: [16 x i8] c"\0D\97\AD\B1\C6<\0C*\C3\CB\F6\EA\95\C3\0Fa", linkage: external, pruned: false)
;CHECK: !4 = !TicketNode(name: "b", digest: [16 x i8] c"X0y\87m.\F8\08\94k$\D9\16\EFh\06", linkage: internal, pruned: false)
;CHECK: !5 = !TicketNode(name: "vp_b", digest: [16 x i8] c"\F7\C2N\957$:\F4bd\D0\AE:\92S&", linkage: external, pruned: false)
;CHECK: !6 = !TicketNode(name: "foo", digest: [16 x i8] c"OUw\D0\E9\98\AF\0C\1D\E0\11cU\A5\AD\C6", linkage: internal, pruned: false)
;CHECK: !7 = !TicketNode(name: "bar", digest: [16 x i8] c"g\8E3\06\C3\DC\A1\A4\C5(\17\80\E3\0F\0A\E5", linkage: internal, pruned: false)
