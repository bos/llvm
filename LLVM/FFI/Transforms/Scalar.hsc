{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module LLVM.FFI.Transforms.Scalar where

import LLVM.FFI.Core

foreign import ccall unsafe "LLVMAddCFGSimplificationPass" addCFGSimplificationPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddConstantPropagationPass" addConstantPropagationPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddDemoteMemoryToRegisterPass" addDemoteMemoryToRegisterPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddGVNPass" addGVNPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddInstructionCombiningPass" addInstructionCombiningPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddPromoteMemoryToRegisterPass" addPromoteMemoryToRegisterPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddReassociatePass" addReassociatePass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddAggressiveDCEPass" addAggressiveDCEPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddDeadStoreEliminationPass" addDeadStoreEliminationPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddIndVarSimplifyPass" addIndVarSimplifyPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddJumpThreadingPass" addJumpThreadingPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddLICMPass" addLICMPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddLoopDeletionPass" addLoopDeletionPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddLoopIndexSplitPass" addLoopIndexSplitPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddLoopRotatePass" addLoopRotatePass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddLoopUnrollPass" addLoopUnrollPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddLoopUnswitchPass" addLoopUnswitchPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddMemCpyOptPass" addMemCpyOptPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddSCCPPass" addSCCPPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddScalarReplAggregatesPass" addScalarReplAggregatesPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddSimplifyLibCallsPass" addSimplifyLibCallsPass
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddTailCallEliminationPass" addTailCallEliminationPass
    :: PassManagerRef -> IO ()
