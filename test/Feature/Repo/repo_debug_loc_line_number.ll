; Test for the debug line information, which is considered to the repo hash calculation.
;
; Inputs/Foo,Bar are used as reference functions. If two functions have the same code
; but different line number (added some empty lines in the function body), they have
; different hash value.
;
; Corresponding the source code:
;
; int Foo() {
;   /*column change*/
;   return 1;
; }
; void Bar() {
;   Foo();
; }
;
; The testcase includes three steps:
; Step 1: Generate the repo IR code which contains the TicketNode metadata (Inputs/Foo,Bar);
; Step 2: Create the database 'clang.db' which contains all Tickets.
; Step 3: Run 'opt' cmd for this file, check the funciton 'Fn' is not optimised;

; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S -mtriple x86_64-pc-linux-gnu-repo %S/Inputs/repo_debug_loc.ll -o %t 2>&1
; RUN: env REPOFILE=%t.db llc -filetype=obj %t -o /dev/null 2>&1
; RUN: env REPOFILE=%t.db opt -S -mtriple x86_64-pc-linux-gnu-repo %s | FileCheck %s

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
!8 = !DILocation(line: 3, column: 3, scope: !6)
!9 = distinct !DISubprogram(name: "Bar", scope: !1, file: !1, line: 5, type: !7, isLocal: false, isDefinition: true, scopeLine: 6, isOptimized: false, unit: !0, retainedNodes: !2)
!10 = !DILocation(line: 6, column: 3, scope: !9)
!11 = !DILocation(line: 7, column: 1, scope: !9)

;CHECK: !TicketNode(name: "Foo", digest: [16 x i8] c"{{.+}}", linkage: external, pruned: false)
;CHECK: !TicketNode(name: "Bar", digest: [16 x i8] c"{{.+}}", linkage: external, pruned: false)
