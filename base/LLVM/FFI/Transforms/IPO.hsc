{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module LLVM.FFI.Transforms.IPO where

import LLVM.FFI.Core

foreign import ccall unsafe "LLVMAddArgumentPromotionPass" addArgumentPromotionPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddConstantMergePass" addConstantMergePass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddDeadArgEliminationPass" addDeadArgEliminationPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddFunctionAttrsPass" addFunctionAttrsPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddFunctionInliningPass" addFunctionInliningPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddGlobalDCEPass" addGlobalDCEPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddGlobalOptimizerPass" addGlobalOptimizerPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddIPConstantPropagationPass" addIPConstantPropagationPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddPruneEHPass" addPruneEHPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddIPSCCPPass" addIPSCCPPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddStripDeadPrototypesPass" addStripDeadPrototypesPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddStripSymbolsPass" addStripSymbolsPass
    :: PassManagerRef -> IO ()
