; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S %s -o %t
; RUN:  env REPOFILE=%t.db llc -filetype=obj %t -o %t2

target triple = "x86_64-pc-linux-gnu-repo"

@i = thread_local global i32 1, align 4
@j = thread_local global i32 2, align 4

define i32 @_Z3getv(){
entry:
  %0 = load i32, i32* @i, align 4
  %1 = load i32, i32* @j, align 4
  %add = add nsw i32 %0, %1
  ret i32 %add
}
