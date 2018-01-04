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
  ret i32 1
}

;CHECK: !0 = !TicketNode(name: "fp_foo", digest: [16 x i8] c"\0C5\085\13~v\08\D0]{`\8FzI\E3", linkage: external)
;CHECK: !1 = !TicketNode(name: "fp_bar", digest: [16 x i8] c"\0C\D6\A6\05\FE}l\0Bw)\9B\AE\10\12\E2$", linkage: external)
;CHECK: !2 = !TicketNode(name: "foo", digest: [16 x i8] c"\9E\FC16\E2\F6\1A\F5\F5av\A5\9F\05b\D3", linkage: internal)
;CHECK: !3 = !TicketNode(name: "bar", digest: [16 x i8] c"\9E\FC16\E2\F6\1A\F5\F5av\A5\9F\05b\D3", linkage: internal)
