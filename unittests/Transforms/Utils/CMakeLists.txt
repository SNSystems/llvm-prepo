set(LLVM_LINK_COMPONENTS
  Analysis
  AsmParser
  Core
  Support
  TransformUtils
  )

add_llvm_unittest(UtilsTests
  ASanStackFrameLayoutTest.cpp
  BasicBlockUtils.cpp
  Cloning.cpp
  CodeExtractor.cpp
  FunctionComparator.cpp
  RepoHashCalculator.cpp
  IntegerDivision.cpp
  Local.cpp
  OrderedInstructions.cpp
  SSAUpdaterBulk.cpp
  ValueMapperTest.cpp
  )
