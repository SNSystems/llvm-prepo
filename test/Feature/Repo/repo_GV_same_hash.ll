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

;CHECK: !0 = !TicketNode(name: "fp_foo", digest: [16 x i8] c"<c\930z>i>\AFXi\17G[\A1\A0", linkage: external)
;CHECK: !1 = !TicketNode(name: "fp_bar", digest: [16 x i8] c"\9F\F2!\9F}(\D8\83\EAS\9E>\CE\F4\B5\B1", linkage: external)
;CHECK: !2 = !TicketNode(name: "a", digest: [16 x i8] c"Wb|\8D\9B\03\D9\D3\F0\D7\0E\92l=\E8\D0", linkage: internal)
;CHECK: !3 = !TicketNode(name: "vp_a", digest: [16 x i8] c"z\80\E7\D5-\E3\BE*?\88\E1o\BA*\DF\BB", linkage: external)
;CHECK: !4 = !TicketNode(name: "b", digest: [16 x i8] c"Wb|\8D\9B\03\D9\D3\F0\D7\0E\92l=\E8\D0", linkage: internal)
;CHECK: !5 = !TicketNode(name: "vp_b", digest: [16 x i8] c"8a\F3\DF\F9\E1M\1A\FE\98\BDf\8B\CF\F3\06", linkage: external)
;CHECK: !6 = !TicketNode(name: "foo", digest: [16 x i8] c"\F1\E3\F1\0DG\D1Z\9C\BC\01\BF_+Z8L", linkage: internal)
;CHECK: !7 = !TicketNode(name: "bar", digest: [16 x i8] c"\F1\E3\F1\0DG\D1Z\9C\BC\01\BF_+Z8L", linkage: internal)
