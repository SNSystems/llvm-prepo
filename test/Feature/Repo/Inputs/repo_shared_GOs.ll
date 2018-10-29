target triple = "x86_64-pc-linux-gnu-repo"

define i32 @Fn() {
entry:
  ret i32 1
}

define available_externally i32 @foo() {
  ret i32 1
}

define void @call_foo() {
  %call = call i32 @foo()
  ret void
}
