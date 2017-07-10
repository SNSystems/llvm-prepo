; RUN: opt -mtriple="x86_64-pc-linux-gnu-repo" -O0 -S %s | FileCheck %s
; RUN: opt -mtriple="x86_64-pc-linux-gnu-repo" -S %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-elf"

;CHECK: @c = global i32 8, align 4, !fragment !0
@c = global i32 8, align 4
;CHECK: @a = internal global i32 1, align 4, !fragment !1
@a = internal global i32 1, align 4
;CHECK: @b = external global i32, align 4, !fragment !2
@b = internal global i32 1, align 4

;CHECK: define i8* @me() !fragment !3
define i8* @me() {
entry:
  ret i8* bitcast (i8* ()* @me to i8*)
}

;CHECK: declare !fragment !4 i32 @foo()
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

;CHECK: !0 = !TicketNode(name: "c", digest: [16 x i8] c"7\A3o\05=e\BE\A70\DE\F10X^\E8\88", linkage: 0)
;CHECK: !1 = !TicketNode(name: "a", digest: [16 x i8] c"\9E\E2\DB\EB_\82r\FF\F3\08\D9\85\B70\C2k", linkage: 7)
;CHECK: !2 = !TicketNode(name: "b", digest: [16 x i8] c"\9E\E2\DB\EB_\82r\FF\F3\08\D9\85\B70\C2k", linkage: 7)
;CHECK: !3 = !TicketNode(name: "me", digest: [16 x i8] c"\A5\EB\A4LL+\AFZ\03\1C\17\DC\90\D2\B3\E9", linkage: 0)
;CHECK: !4 = !TicketNode(name: "foo", digest: [16 x i8] c"\C7\17<\9Eg\0F\87\D4t\7F\1B\F3\F2\9D\82&", linkage: 0)
;CHECK: !5 = !TicketNode(name: "bar", digest: [16 x i8] c"\E2\88ClF\94k1da\FF\91\D8Z\D4\F5", linkage: 0)
