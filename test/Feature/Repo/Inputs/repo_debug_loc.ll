; Foo and Bar are used as reference functions.
;
; Corresponding the source code:
;
; int Foo() {
;   return 1;
; }
;
; void Bar() {
;   Foo();
; }
;

target triple = "x86_64-pc-linux-gnu-elf"

define dso_local i32 @Foo() !dbg !6 {
entry:
  ret i32 1, !dbg !8
}

define dso_local void @Bar() !dbg !9 {
entry:
  %call = call i32 @Foo(), !dbg !10
  ret void, !dbg !11
}

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang", isOptimized: false, runtimeVersion: 0, emissionKind: LineTablesOnly, enums: !2)
!1 = !DIFile(filename: "Fn.c", directory: "C:\5CMyWork\5Crepo_bug\5Cbug41")
!2 = !{}
!3 = !{i32 2, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = distinct !DISubprogram(name: "Foo", scope: !1, file: !1, line: 1, type: !7, isLocal: false, isDefinition: true, scopeLine: 1, isOptimized: false, unit: !0, retainedNodes: !2)
!7 = !DISubroutineType(types: !2)
!8 = !DILocation(line: 2, column: 3, scope: !6)
!9 = distinct !DISubprogram(name: "Bar", scope: !1, file: !1, line: 5, type: !7, isLocal: false, isDefinition: true, scopeLine: 5, isOptimized: false, unit: !0, retainedNodes: !2)
!10 = !DILocation(line: 6, column: 3, scope: !9)
!11 = !DILocation(line: 7, column: 1, scope: !9)

