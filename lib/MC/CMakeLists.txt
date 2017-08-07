
add_llvm_library(LLVMMC
  ConstantPools.cpp
  ELFObjectWriter.cpp
  MCAsmBackend.cpp
  MCAsmInfo.cpp
  MCAsmInfoCOFF.cpp
  MCAsmInfoDarwin.cpp
  MCAsmInfoELF.cpp
  MCAsmInfoRepo.cpp
  MCAsmInfoWasm.cpp
  MCAsmMacro.cpp
  MCAsmStreamer.cpp
  MCAssembler.cpp
  MCCodeEmitter.cpp
  MCCodePadder.cpp
  MCCodeView.cpp
  MCContext.cpp
  MCDwarf.cpp
  MCELFObjectTargetWriter.cpp
  MCELFStreamer.cpp
  MCExpr.cpp
  MCFragment.cpp
  MCInst.cpp
  MCInstPrinter.cpp
  MCInstrAnalysis.cpp
  MCInstrDesc.cpp
  MCLabel.cpp
  MCLinkerOptimizationHint.cpp
  MCMachOStreamer.cpp
  MCMachObjectTargetWriter.cpp
  MCNullStreamer.cpp
  MCObjectFileInfo.cpp
  MCObjectStreamer.cpp
  MCObjectWriter.cpp
  MCRegisterInfo.cpp
  MCRepoStreamer.cpp
  MCRepoObjectTargetWriter.cpp
  MCSchedule.cpp
  MCSection.cpp
  MCSectionCOFF.cpp
  MCSectionELF.cpp
  MCSectionMachO.cpp
  MCSectionRepo.cpp
  MCSectionWasm.cpp
  MCStreamer.cpp
  MCSubtargetInfo.cpp
  MCSymbol.cpp
  MCSymbolELF.cpp
  MCTargetOptions.cpp
  MCValue.cpp
  MCWasmObjectTargetWriter.cpp
  MCWasmStreamer.cpp
  MCWin64EH.cpp
  MCWinCOFFStreamer.cpp
  MCWinEH.cpp
  MachObjectWriter.cpp
  RepoObjectWriter.cpp
  StringTableBuilder.cpp
  SubtargetFeature.cpp
  WasmObjectWriter.cpp
  WinCOFFObjectWriter.cpp

  MCRepoFragment/MCRepoFragment.cpp

  ADDITIONAL_HEADER_DIRS
  ${LLVM_MAIN_INCLUDE_DIR}/llvm/MC
  ${LLVM_MAIN_INCLUDE_DIR}/llvm/MC/MCRepoFragment

DEPENDS
pstore
  )

add_subdirectory(MCParser)
add_subdirectory(MCDisassembler)

target_link_libraries (LLVMMC PRIVATE pstore)
#set_property(GLOBAL APPEND PROPERTY LLVM_EXPORTS pstore)
