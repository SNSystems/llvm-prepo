; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db opt -S %s -o %t
; RUN: env REPOFILE=%t.db llc -filetype=obj %t -o %t1

target triple = "x86_64-pc-linux-gnu-repo"

@bar = weak_odr global i32 42

define weak_odr i32*  @foo() {
  ret i32* @bar
}
