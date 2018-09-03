; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S %s | FileCheck %s

target triple = "x86_64-pc-linux-gnu-repo"

%struct.Struct = type { i32, float }

@globalIntZero = global i32 0, align 4
@globalIntArrayZero = global [2 x i32] zeroinitializer, align 4
@globalFloatZero = global float 0.000000e+00, align 4
@globalFloatArrayZero = global [2 x float] zeroinitializer, align 4
@globalDoubleZero = global double 0.000000e+00, align 8
@globalDoubleArrayZero = global [2 x double] zeroinitializer, align 16
@globalStructNull = global %struct.Struct zeroinitializer, align 4
@globalInt = common global i32 0, align 4
@globalIntArray = common global [2 x i32] zeroinitializer, align 4
@globalFloat = common global float 0.000000e+00, align 4
@globalFloatArray = common global [2 x float] zeroinitializer, align 4
@globalDouble = common global double 0.000000e+00, align 8
@globalDoubleArray = common global [2 x double] zeroinitializer, align 16
@globalStruct = common global %struct.Struct zeroinitializer, align 4

;CHECK:      !0 = !TicketNode(name: "globalIntZero", digest: [16 x i8] c"\BFJ!m\C3\DF\C14\C1\FD\D8>@P(\C9", linkage: external, pruned: false)
;CHECK-NEXT: !1 = !TicketNode(name: "globalIntArrayZero", digest: [16 x i8] c"w?\F0\B7\D2\15\EDB\A8v}]\D2\A2><", linkage: external, pruned: false)
;CHECK-NEXT: !2 = !TicketNode(name: "globalFloatZero", digest: [16 x i8] c"'E\EFT\D8\84\C4\AC\89mYkO\F6\E56", linkage: external, pruned: false)
;CHECK-NEXT: !3 = !TicketNode(name: "globalFloatArrayZero", digest: [16 x i8] c"\A0p\B0\A1\9C\9D5\D5\FB\9E\0A\13o\DB\9D?", linkage: external, pruned: false)
;CHECK-NEXT: !4 = !TicketNode(name: "globalDoubleZero", digest: [16 x i8] c"P\18\D4KBK\0C\F1=\84\83Oi\BF\B4\87", linkage: external, pruned: false)
;CHECK-NEXT: !5 = !TicketNode(name: "globalDoubleArrayZero", digest: [16 x i8] c"\0Doh\E7\C0\C2Pz\9A\88\9E\DEAV\9A\95", linkage: external, pruned: false)
;CHECK-NEXT: !6 = !TicketNode(name: "globalStructNull", digest: [16 x i8] c"04J\C7\E9\F9T\16\C0_]\F2\E2k\9C\9B", linkage: external, pruned: false)
;CHECK-NEXT: !7 = !TicketNode(name: "globalInt", digest: [16 x i8] c"\BFJ!m\C3\DF\C14\C1\FD\D8>@P(\C9", linkage: common, pruned: false)
;CHECK-NEXT: !8 = !TicketNode(name: "globalIntArray", digest: [16 x i8] c"w?\F0\B7\D2\15\EDB\A8v}]\D2\A2><", linkage: common, pruned: false)
;CHECK-NEXT: !9 = !TicketNode(name: "globalFloat", digest: [16 x i8] c"'E\EFT\D8\84\C4\AC\89mYkO\F6\E56", linkage: common, pruned: false)
;CHECK-NEXT: !10 = !TicketNode(name: "globalFloatArray", digest: [16 x i8] c"\A0p\B0\A1\9C\9D5\D5\FB\9E\0A\13o\DB\9D?", linkage: common, pruned: false)
;CHECK-NEXT: !11 = !TicketNode(name: "globalDouble", digest: [16 x i8] c"P\18\D4KBK\0C\F1=\84\83Oi\BF\B4\87", linkage: common, pruned: false)
;CHECK-NEXT: !12 = !TicketNode(name: "globalDoubleArray", digest: [16 x i8] c"\0Doh\E7\C0\C2Pz\9A\88\9E\DEAV\9A\95", linkage: common, pruned: false)
;CHECK-NEXT: !13 = !TicketNode(name: "globalStruct", digest: [16 x i8] c"04J\C7\E9\F9T\16\C0_]\F2\E2k\9C\9B", linkage: common, pruned: false)
