; RUN: opt -S %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

@fp_foo = global [1 x i32 (...)*] [i32 (...)* bitcast (i32 ()* @foo to i32 (...)*)], align 8
@fp_bar = global [1 x i32 (...)*] [i32 (...)* bitcast (i32 ()* @bar to i32 (...)*)], align 8

define internal i32 @foo() {
entry:
  ret i32 1
}

define internal i32 @bar() {
entry:
  ret i32 2
}

;CHECK: !0 = !TicketNode(name: "fp_foo", digest: [16 x i8] c"WBo8\B9K\0D\98\D6\B8\C5-Q\F5B^", linkage: external)
;CHECK: !1 = !TicketNode(name: "fp_bar", digest: [16 x i8] c"\1F\B2\E7V\C9$:\AD\D1QrT4\08\AB\07", linkage: external)
;CHECK: !2 = !TicketNode(name: "foo", digest: [16 x i8] c"\9E\FC16\E2\F6\1A\F5\F5av\A5\9F\05b\D3", linkage: internal)
;CHECK: !3 = !TicketNode(name: "bar", digest: [16 x i8] c"f\D1S\E1\14\93\B1\7FhB\17\C8\C1tV5", linkage: internal)
