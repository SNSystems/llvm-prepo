set(LLVM_LINK_COMPONENTS
    MC
    Object
    Support
)
add_llvm_tool (repo2obj
    repo2obj.cpp
    R2OELFOutputSection.cpp
    R2OELFSectionType.cpp
    R2OELFStringTable.cpp
    WriteHelpers.cpp
)
target_link_libraries (repo2obj PRIVATE pstore-core)
#eof CMakeLists.txt
