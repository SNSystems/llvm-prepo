; RUN: opt -S %s | FileCheck %s

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

;CHECK: !0 = !TicketNode(name: "globalIntZero", digest: [16 x i8] c"\AD\B1\D8\F7Fs\08C\87\9E\85\DC\02w\DAD", linkage: external)
;CHECK-NEXT: !1 = !TicketNode(name: "globalIntArrayZero", digest: [16 x i8] c"\1ExQ@%u\A7\00@\C9\D7\A3B\AC\F5/", linkage: external)
;CHECK-NEXT: !2 = !TicketNode(name: "globalFloatZero", digest: [16 x i8] c"Y\EB\F7\F1\E5+\BD=\19Ex\F4_\B0i\9F", linkage: external)
;CHECK-NEXT: !3 = !TicketNode(name: "globalFloatArrayZero", digest: [16 x i8] c"j\18\97\A7\C9\17\DF\CD\C8\E3\A0\90\DE\BE^\02", linkage: external)
;CHECK-NEXT: !4 = !TicketNode(name: "globalDoubleZero", digest: [16 x i8] c"7\F4\B7pZf\F9Q\EE\E2\83>\22E\B1\BB", linkage: external)
;CHECK-NEXT: !5 = !TicketNode(name: "globalDoubleArrayZero", digest: [16 x i8] c"<E\A4\8E6\9Be\E7\E1\98\B3?=\02\C3\17", linkage: external)
;CHECK-NEXT: !6 = !TicketNode(name: "globalStructNull", digest: [16 x i8] c"G!!\F9Nbu:\18\17U\F8\BB0*\C6", linkage: external)
;CHECK-NEXT: !7 = !TicketNode(name: "globalInt", digest: [16 x i8] c"\AD\B1\D8\F7Fs\08C\87\9E\85\DC\02w\DAD", linkage: common)
;CHECK-NEXT: !8 = !TicketNode(name: "globalIntArray", digest: [16 x i8] c"\1ExQ@%u\A7\00@\C9\D7\A3B\AC\F5/", linkage: common)
;CHECK-NEXT: !9 = !TicketNode(name: "globalFloat", digest: [16 x i8] c"Y\EB\F7\F1\E5+\BD=\19Ex\F4_\B0i\9F", linkage: common)
;CHECK-NEXT: !10 = !TicketNode(name: "globalFloatArray", digest: [16 x i8] c"j\18\97\A7\C9\17\DF\CD\C8\E3\A0\90\DE\BE^\02", linkage: common)
;CHECK-NEXT: !11 = !TicketNode(name: "globalDouble", digest: [16 x i8] c"7\F4\B7pZf\F9Q\EE\E2\83>\22E\B1\BB", linkage: common)
;CHECK-NEXT: !12 = !TicketNode(name: "globalDoubleArray", digest: [16 x i8] c"<E\A4\8E6\9Be\E7\E1\98\B3?=\02\C3\17", linkage: common)
;CHECK-NEXT: !13 = !TicketNode(name: "globalStruct", digest: [16 x i8] c"G!!\F9Nbu:\18\17U\F8\BB0*\C6", linkage: common)