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

;CHECK:      !0 = !TicketNode(name: "globalIntZero", digest: [16 x i8] c"~AQ\0E\CB>Z5\CB \FF\05 \A9\C6b", linkage: external, pruned: false)
;CHECK-NEXT: !1 = !TicketNode(name: "globalIntArrayZero", digest: [16 x i8] c"\0B\E5U\00\1F_\CB\93&g\BC/\04o \11", linkage: external, pruned: false)
;CHECK-NEXT: !2 = !TicketNode(name: "globalFloatZero", digest: [16 x i8] c"\CB-\CF\0An\93z\1AB\A85\0F\B05F=", linkage: external, pruned: false)
;CHECK-NEXT: !3 = !TicketNode(name: "globalFloatArrayZero", digest: [16 x i8] c"\CA\0A\96\971\22\0Cpj\DB\F89R\9A\05\D4", linkage: external, pruned: false)
;CHECK-NEXT: !4 = !TicketNode(name: "globalDoubleZero", digest: [16 x i8] c"\D4\AF\E0\DB\98w\D3\FA\A3L\E3\FCR\CB\F8\9F", linkage: external, pruned: false)
;CHECK-NEXT: !5 = !TicketNode(name: "globalDoubleArrayZero", digest: [16 x i8] c"\18\DC\C7\1B\9E^K\AF\E7\0E\CF\9F\11\04\22q", linkage: external, pruned: false)
;CHECK-NEXT: !6 = !TicketNode(name: "globalStructNull", digest: [16 x i8] c"\95\81\9Dp\1A\BAh\E0\CE&}WX\B4\89\FD", linkage: external, pruned: false)
;CHECK-NEXT: !7 = !TicketNode(name: "globalInt", digest: [16 x i8] c"~AQ\0E\CB>Z5\CB \FF\05 \A9\C6b", linkage: common, pruned: false)
;CHECK-NEXT: !8 = !TicketNode(name: "globalIntArray", digest: [16 x i8] c"\0B\E5U\00\1F_\CB\93&g\BC/\04o \11", linkage: common, pruned: false)
;CHECK-NEXT: !9 = !TicketNode(name: "globalFloat", digest: [16 x i8] c"\CB-\CF\0An\93z\1AB\A85\0F\B05F=", linkage: common, pruned: false)
;CHECK-NEXT: !10 = !TicketNode(name: "globalFloatArray", digest: [16 x i8] c"\CA\0A\96\971\22\0Cpj\DB\F89R\9A\05\D4", linkage: common, pruned: false)
;CHECK-NEXT: !11 = !TicketNode(name: "globalDouble", digest: [16 x i8] c"\D4\AF\E0\DB\98w\D3\FA\A3L\E3\FCR\CB\F8\9F", linkage: common, pruned: false)
;CHECK-NEXT: !12 = !TicketNode(name: "globalDoubleArray", digest: [16 x i8] c"\18\DC\C7\1B\9E^K\AF\E7\0E\CF\9F\11\04\22q", linkage: common, pruned: false)
;CHECK-NEXT: !13 = !TicketNode(name: "globalStruct", digest: [16 x i8] c"\95\81\9Dp\1A\BAh\E0\CE&}WX\B4\89\FD", linkage: common, pruned: false)
