; RUN: opt -S %s | FileCheck %s

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

;CHECK: !0 = !TicketNode(name: "fp_foo", digest: [16 x i8] c"_\C9\D7\18\C6\5C\94\FAQY\F1\09^\7FL\18", linkage: external)
;CHECK: !1 = !TicketNode(name: "fp_bar", digest: [16 x i8] c"\AD\12\07\EF\DD\BD\0B\C3\E8\EB5\00\B7\0D\AA\C7", linkage: external)
;CHECK: !2 = !TicketNode(name: "a", digest: [16 x i8] c"j97\FBR\0F\95*\98=\F71\8Do\8A\F3", linkage: internal)
;CHECK: !3 = !TicketNode(name: "vp_a", digest: [16 x i8] c"uG\A2\EEj\04\03V\A7\18\7Fy|\DFt\C2", linkage: external)
;CHECK: !4 = !TicketNode(name: "b", digest: [16 x i8] c"j97\FBR\0F\95*\98=\F71\8Do\8A\F3", linkage: internal)
;CHECK: !5 = !TicketNode(name: "vp_b", digest: [16 x i8] c"\141O\87m93\12\B6\5CD\F0.xY\84", linkage: external)
;CHECK: !6 = !TicketNode(name: "foo", digest: [16 x i8] c"\9E\FC16\E2\F6\1A\F5\F5av\A5\9F\05b\D3", linkage: internal)
;CHECK: !7 = !TicketNode(name: "bar", digest: [16 x i8] c"\9E\FC16\E2\F6\1A\F5\F5av\A5\9F\05b\D3", linkage: internal)
