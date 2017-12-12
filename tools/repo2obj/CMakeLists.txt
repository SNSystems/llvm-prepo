set(LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  Object
  Support)
add_llvm_tool (repo2obj repo2obj.cpp R2OELFOutputSection.cpp R2OELFStringTable.cpp)
target_link_libraries (repo2obj pstore)
#eof CMakeLists.txt
