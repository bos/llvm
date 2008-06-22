{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module LLVM.FFI.Transforms.Scalar(
       addCFGSimplificationPass
     , addConstantPropagationPass
     , addDemoteMemoryToRegisterPass
     , addGVNPass
     , addInstructionCombiningPass
     , addPromoteMemoryToRegisterPass
     , addReassociatePass
     ) where

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

