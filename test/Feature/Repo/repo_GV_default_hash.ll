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

;CHECK:      !0 = !TicketNode(name: "globalIntZero", digest: [16 x i8] c"{{.+}}", linkage: external, pruned: false)
;CHECK-NEXT: !1 = !TicketNode(name: "globalIntArrayZero", digest: [16 x i8] c"{{.+}}", linkage: external, pruned: false)
;CHECK-NEXT: !2 = !TicketNode(name: "globalFloatZero", digest: [16 x i8] c"{{.+}}", linkage: external, pruned: false)
;CHECK-NEXT: !3 = !TicketNode(name: "globalFloatArrayZero", digest: [16 x i8] c"{{.+}}", linkage: external, pruned: false)
;CHECK-NEXT: !4 = !TicketNode(name: "globalDoubleZero", digest: [16 x i8] c"{{.+}}", linkage: external, pruned: false)
;CHECK-NEXT: !5 = !TicketNode(name: "globalDoubleArrayZero", digest: [16 x i8] c"{{.+}}", linkage: external, pruned: false)
;CHECK-NEXT: !6 = !TicketNode(name: "globalStructNull", digest: [16 x i8] c"{{.+}}", linkage: external, pruned: false)
;CHECK-NEXT: !7 = !TicketNode(name: "globalInt", digest: [16 x i8] c"{{.+}}", linkage: common, pruned: true)
;CHECK-NEXT: !8 = !TicketNode(name: "globalIntArray", digest: [16 x i8] c"{{.+}}", linkage: common, pruned: true)
;CHECK-NEXT: !9 = !TicketNode(name: "globalFloat", digest: [16 x i8] c"{{.+}}", linkage: common, pruned: true)
;CHECK-NEXT: !10 = !TicketNode(name: "globalFloatArray", digest: [16 x i8] c"{{.+}}", linkage: common, pruned: true)
;CHECK-NEXT: !11 = !TicketNode(name: "globalDouble", digest: [16 x i8] c"{{.+}}", linkage: common, pruned: true)
;CHECK-NEXT: !12 = !TicketNode(name: "globalDoubleArray", digest: [16 x i8] c"{{.+}}", linkage: common, pruned: true)
;CHECK-NEXT: !13 = !TicketNode(name: "globalStruct", digest: [16 x i8] c"{{.+}}", linkage: common, pruned: true)
