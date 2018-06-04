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

;CHECK: !0 = !TicketNode(name: "fp_foo", digest: [16 x i8] c"o\FE\D6\9F\D1M\C8^\B6\8E\C3rJ\97(\E2", linkage: external, pruned: false)
;CHECK: !1 = !TicketNode(name: "fp_bar", digest: [16 x i8] c"\0Ak\EB\C1\F0\94\87\D0\069\1E\E3\1B\ABAu", linkage: external, pruned: false)
;CHECK: !2 = !TicketNode(name: "a", digest: [16 x i8] c"\D3\22\A4[\80\ED\08`e\80\C9\81 l\EB\EE", linkage: internal, pruned: false)
;CHECK: !3 = !TicketNode(name: "vp_a", digest: [16 x i8] c"?nO=\80\0F\F684\1E\D0\E8\8FO\DB\F5", linkage: external, pruned: false)
;CHECK: !4 = !TicketNode(name: "b", digest: [16 x i8] c"\D3\22\A4[\80\ED\08`e\80\C9\81 l\EB\EE", linkage: internal, pruned: false)
;CHECK: !5 = !TicketNode(name: "vp_b", digest: [16 x i8] c"\88\91\08\D2\B9\D7U\1D-\DB\88\D7,H\FB\07", linkage: external, pruned: false)
;CHECK: !6 = !TicketNode(name: "foo", digest: [16 x i8] c"\C5\D2q*\1B\1E\AEbd V \EF\B1\D6\B6", linkage: internal, pruned: false)
;CHECK: !7 = !TicketNode(name: "bar", digest: [16 x i8] c"\C5\D2q*\1B\1E\AEbd V \EF\B1\D6\B6", linkage: internal, pruned: false)
