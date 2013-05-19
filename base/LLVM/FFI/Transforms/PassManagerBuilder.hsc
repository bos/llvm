{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable #-}
module LLVM.FFI.Transforms.PassManagerBuilder where

import Foreign.C.Types
import Foreign.Ptr (Ptr, FunPtr)
import Data.Typeable(Typeable)

import LLVM.FFI.Core

data PassManagerBuilder
    deriving (Typeable)
type PassManagerBuilderRef = Ptr PassManagerBuilder

foreign import ccall unsafe "LLVMPassManagerBuilderCreate" passManagerBuilderCreate
    :: IO PassManagerBuilderRef

foreign import ccall unsafe "LLVMPassManagerBuilderDispose" passManagerBuilderDispose
    :: PassManagerBuilderRef -> IO ()

foreign import ccall unsafe "&LLVMPassManagerBuilderDispose" ptrPassManagerBuilderDispose
    :: FunPtr (PassManagerBuilderRef -> IO ())

foreign import ccall unsafe "LLVMPassManagerBuilderSetOptLevel" passManagerBuilderSetOptLevel
    :: PassManagerBuilderRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderSetSizeLevel" passManagerBuilderSetSizeLevel
    :: PassManagerBuilderRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderPopulateFunctionPassManager" passManagerBuilderPopulateFunctionPassManager
    :: PassManagerBuilderRef -> PassManagerRef -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderPopulateModulePassManager" passManagerBuilderPopulateModulePassManager
    :: PassManagerBuilderRef -> PassManagerRef -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderPopulateLTOPassManager" passManagerBuilderPopulateLTOPassManager
    :: PassManagerBuilderRef -> PassManagerRef -> CInt -> CInt -> IO ()
