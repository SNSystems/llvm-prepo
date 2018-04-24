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

;CHECK:      !0 = !TicketNode(name: "globalIntZero", digest: [16 x i8] c"e4\A6\A2j\96\9BY\E0\AB$\85\1ED\19\D5", linkage: external, pruned: false)
;CHECK-NEXT: !1 = !TicketNode(name: "globalIntArrayZero", digest: [16 x i8] c"\D5\82s1z\08Q~\9A\DC\90\BE\80\F0\0E\B5", linkage: external, pruned: false)
;CHECK-NEXT: !2 = !TicketNode(name: "globalFloatZero", digest: [16 x i8] c"tQ\0F\EBs5\15*\CD\1Ej&\84\A9\FB\82", linkage: external, pruned: false)
;CHECK-NEXT: !3 = !TicketNode(name: "globalFloatArrayZero", digest: [16 x i8] c"\DF~\D4o\94>\0C9\F1\8Et\CD\CE~@\CD", linkage: external, pruned: false)
;CHECK-NEXT: !4 = !TicketNode(name: "globalDoubleZero", digest: [16 x i8] c"s\93\BC\BA\17\E5\AB4\F4/\9F!m\89\08\EE", linkage: external, pruned: false)
;CHECK-NEXT: !5 = !TicketNode(name: "globalDoubleArrayZero", digest: [16 x i8] c"\15\ED\AC\C0|\1B\15\D0t=b\BE7~`?", linkage: external, pruned: false)
;CHECK-NEXT: !6 = !TicketNode(name: "globalStructNull", digest: [16 x i8] c"@\0CS\88\A3\0Cy\CD`\16\18Y(\F8,<", linkage: external, pruned: false)
;CHECK-NEXT: !7 = !TicketNode(name: "globalInt", digest: [16 x i8] c"e4\A6\A2j\96\9BY\E0\AB$\85\1ED\19\D5", linkage: common, pruned: false)
;CHECK-NEXT: !8 = !TicketNode(name: "globalIntArray", digest: [16 x i8] c"\D5\82s1z\08Q~\9A\DC\90\BE\80\F0\0E\B5", linkage: common, pruned: false)
;CHECK-NEXT: !9 = !TicketNode(name: "globalFloat", digest: [16 x i8] c"tQ\0F\EBs5\15*\CD\1Ej&\84\A9\FB\82", linkage: common, pruned: false)
;CHECK-NEXT: !10 = !TicketNode(name: "globalFloatArray", digest: [16 x i8] c"\DF~\D4o\94>\0C9\F1\8Et\CD\CE~@\CD", linkage: common, pruned: false)
;CHECK-NEXT: !11 = !TicketNode(name: "globalDouble", digest: [16 x i8] c"s\93\BC\BA\17\E5\AB4\F4/\9F!m\89\08\EE", linkage: common, pruned: false)
;CHECK-NEXT: !12 = !TicketNode(name: "globalDoubleArray", digest: [16 x i8] c"\15\ED\AC\C0|\1B\15\D0t=b\BE7~`?", linkage: common, pruned: false)
;CHECK-NEXT: !13 = !TicketNode(name: "globalStruct", digest: [16 x i8] c"@\0CS\88\A3\0Cy\CD`\16\18Y(\F8,<", linkage: common, pruned: false)
