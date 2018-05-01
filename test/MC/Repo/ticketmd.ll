; RUN: rm -f %t.db
; RUN: env REPOFILE=%t.db llc -filetype=obj %s -o %t

target triple = "x86_64-pc-linux-gnu-repo"

define i32 @D() #0 !repo_ticket !0 {
entry:
  ret i32 1
}

define i32 @C() #0 {
entry:
  %call = call i32 @D()
  ret i32 %call
}

define i32 @B() #0 {
entry:
  %call = call i32 @C()
  ret i32 %call
}

define i32 @A() #0 !repo_ticket !1 {
entry:
  %call = call i32 @B()
  ret i32 %call
}

!repo.tickets = !{!0, !1}

!0 = !TicketNode(name: "D", digest: [16 x i8] c"\BDV\AA\B2x\89c~\94\B4I\C8Eo)\AF", linkage: external, pruned: false)
!1 = !TicketNode(name: "A", digest: [16 x i8] c"\B4 k\F6[\B8G\A6\9C\FC\C6/w\19\D3\BE", linkage: external, pruned: false)
