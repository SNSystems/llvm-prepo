; RUN: llvm-as < %s | llvm-dis | llvm-as | llvm-dis | FileCheck %s

; CHECK: @_ZL4fact = internal global i32 1, comdat, align 4, !fragment !0
$_ZL4fact = comdat any
@_ZL4fact = internal global i32 1, align 4, !fragment !0, comdat($_ZL4fact)

; CHECK: define weak i32 @_Z9factorialv() !fragment !1
define weak i32 @_Z9factorialv() !fragment !1 {
entry:
  %0 = load i32, i32* @_ZL4fact, align 4
  ret i32 %0
}

; CHECK: define i32 @main() !fragment !2
define i32 @main() !fragment !2 {
entry:
  %retval = alloca i32, align 4
  store i32 0, i32* %retval, align 4
  %call = call i32 @_Z9factorialv()
  ret i32 %call
}

; CHECK: !0 = !TicketNode(name: "_ZL4fact", digest: [16 x i8] c"\DC\8BWeQ\E4\03\E6\F3:\DE\D1\9F\90\AC\F7", linkage: internal, isComdat: true)
!0 = !TicketNode(name: "_ZL4fact", digest: [16 x i8] c"\DC\8BWeQ\E4\03\E6\F3:\DE\D1\9F\90\AC\F7", linkage: internal, isComdat: true)
; CHECK: !1 = !TicketNode(name: "_Z9factorialv", digest: [16 x i8] c"\7F\D0\A4\88\D0\86r\95Q\DB\EC\E1\94\F9\CF\DD", linkage: extern_weak, isComdat: false)
!1 = !TicketNode(name: "_Z9factorialv", digest: [16 x i8] c"\7F\D0\A4\88\D0\86r\95Q\DB\EC\E1\94\F9\CF\DD", linkage: extern_weak, isComdat: false)
; CHECK: !2 = !TicketNode(name: "main", digest: [16 x i8] c"7)\18\16\EBX\A8P\90\B6\80r\C2\BE\DB\89", linkage: external, isComdat: false)
!2 = !TicketNode(name: "main", digest: [16 x i8] c"7)\18\16\EBX\A8P\90\B6\80r\C2\BE\DB\89", linkage: external, isComdat: false)
