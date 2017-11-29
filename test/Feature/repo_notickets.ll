; RUN: llc -filetype=obj -debug-only repo-object %s -o %t.o

target triple = "x86_64-pc-linux-gnu-repo"

!llvm.module.flags = !{!0}

!0 = !{i32 1, !"wchar_size", i32 4}

