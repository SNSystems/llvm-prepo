; RUN: opt -S %s | FileCheck %s

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

;CHECK: !0 = !TicketNode(name: "fp_foo", digest: [16 x i8] c"\D1\F38W\C8[\FE\D1b\1B\D4\08\05hd\A9", linkage: external, pruned: false)
;CHECK: !1 = !TicketNode(name: "fp_bar", digest: [16 x i8] c"[d\93\860\9Ca\A6\E7\DE\BEp\CBq2&", linkage: external, pruned: false)
;CHECK: !2 = !TicketNode(name: "a", digest: [16 x i8] c"\FF\8By\DD\A10N%\A1X/?<9\C0E", linkage: internal, pruned: false)
;CHECK: !3 = !TicketNode(name: "vp_a", digest: [16 x i8] c"\04x\14\B0\D3\09]D;,X\EE,\FB\C8a", linkage: external, pruned: false)
;CHECK: !4 = !TicketNode(name: "b", digest: [16 x i8] c"\98\B70\A7)>\AC\118^\8BM\C1\B2U\97", linkage: internal, pruned: false)
;CHECK: !5 = !TicketNode(name: "vp_b", digest: [16 x i8] c"\B0\AC\97&[\A6\DD\D6OW\1A(9L\12\F3", linkage: external, pruned: false)
;CHECK: !6 = !TicketNode(name: "foo", digest: [16 x i8] c"\B5\B00\055\C0j\D0s\150<\0B&\C5\07", linkage: internal, pruned: false)
;CHECK: !7 = !TicketNode(name: "bar", digest: [16 x i8] c"E\DB\FDg\86z\F5)\07\109%\FD\FF\04\83", linkage: internal, pruned: false)
