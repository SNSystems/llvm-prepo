; RUN: llc -filetype=obj %s -o %t

target triple = "x86_64-pc-linux-gnu-repo"

@i = external thread_local global i32, align 4, !repo_ticket !0
@j = thread_local global i32 2, align 4, !repo_ticket !1

define i32 @_Z3getv() !repo_ticket !2 {
entry:
  %0 = load i32, i32* @i, align 4
  %1 = load i32, i32* @j, align 4
  %add = add nsw i32 %0, %1
  ret i32 %add
}

!repo.tickets = !{!0, !1, !2}

!0 = !TicketNode(name: "i", digest: [16 x i8] c"\89\1D&J\B8\87U\15\EA\80#/7\12\8E\A2", linkage: external)
!1 = !TicketNode(name: "j", digest: [16 x i8] c"\86\1B\FAU\A5*=~\B1\CC^\89)mbV", linkage: external)
!2 = !TicketNode(name: "_Z3getv", digest: [16 x i8] c"\DC\E7$d\FAX\B3|AX\A5K?,N\C7", linkage: external)

