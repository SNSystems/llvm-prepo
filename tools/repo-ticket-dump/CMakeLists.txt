set(LLVM_LINK_COMPONENTS
    MC
    Object
    Support
)
add_llvm_tool (repo-ticket-dump
    RepoTicketDump.cpp
)
target_link_libraries (repo-ticket-dump pstore)
#eof CMakeLists.txt

