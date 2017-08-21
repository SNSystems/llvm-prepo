set(LLVM_LINK_COMPONENTS
  ${LLVM_TARGETS_TO_BUILD}
  MC
  MCDisassembler
  Support
  )

add_llvm_unittest(MCTests
  Disassembler.cpp
  DwarfLineTables.cpp
  RepoAligned.cpp
  RepoFragment.cpp
  RepoSparseArray.cpp
  StringTableBuilderTest.cpp
  TargetRegistry.cpp
  )

target_link_libraries (MCTests pstore)
