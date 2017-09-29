; RUN: opt -mtriple="x86_64-pc-linux-gnu-repo" -O0 -S %s | FileCheck %s
; RUN: opt -mtriple="x86_64-pc-linux-gnu-repo" -S %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-elf"

;CHECK: @c = global i32 8, comdat, align 4, !fragment !0
$c = comdat any
@c = global i32 8, align 4, comdat($c)
;CHECK: @a = internal global i32 1, comdat, align 4, !fragment !1
$a = comdat exactmatch
@a = internal global i32 1, align 4, comdat($a)
;CHECK: @b = internal global i32 1, comdat, align 4, !fragment !2
$b = comdat largest
@b = internal global i32 1, comdat, align 4, comdat($b)

;CHECK: define i8* @me() comdat !fragment !3
$me = comdat noduplicates
define i8* @me() comdat($me) {
entry:
  ret i8* bitcast (i8* ()* @me to i8*)
}

;CHECK: define i32 @foo() !fragment !4
define i32 @foo() {
entry:
  %0 = load i32, i32* @a, align 4
  %inc = add nsw i32 %0, 1
  store i32 %inc, i32* @a, align 4
  store i32 2, i32* @b, align 4
  store i32 2, i32* @c, align 4
  %1 = load i32, i32* @a, align 4
  ret i32 %1
}

;CHECK: define i32 @bar() !fragment !5
define i32 @bar() {
entry:
  %call = call i32 @foo()
  ret i32 %call
}

;CHECK: !0 = !TicketNode(name: "c", digest: [16 x i8] c"\FAmU\15A\0A}TK\FA\BA\12X\B6\D8N", linkage: 0, isComdat: true)
;CHECK: !1 = !TicketNode(name: "a", digest: [16 x i8] c"\E6B\07mq\05\E3>\8C\EC\F5G|\D0\D8k", linkage: 7, isComdat: true)
;CHECK: !2 = !TicketNode(name: "b", digest: [16 x i8] c"\E6B\07mq\05\E3>\8C\EC\F5G|\D0\D8k", linkage: 7, isComdat: true)
;CHECK: !3 = !TicketNode(name: "me", digest: [16 x i8] c"\036{\09\84j\22\B6\A2$\13)\12\9B\C6\D1", linkage: 0, isComdat: true)
;CHECK: !4 = !TicketNode(name: "foo", digest: [16 x i8] c"_?E&\AC$sE\DAvYk\A5\E4\83)", linkage: 0, isComdat: false)
;CHECK: !5 = !TicketNode(name: "bar", digest: [16 x i8] c"\B7\B1K\5Cj\8D*T\22\13|q:\EE\9D\A6", linkage: 0, isComdat: false)
