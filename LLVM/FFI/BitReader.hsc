{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module LLVM.FFI.BitReader where
import Foreign.C.String(CString)
import Foreign.C.Types(CInt)
import Foreign.Ptr(Ptr)

import LLVM.FFI.Core

foreign import ccall unsafe "LLVMGetBitcodeModuleProvider" getBitcodeModuleProvider
    :: MemoryBufferRef -> (Ptr ModuleProviderRef) -> (Ptr CString) -> IO CInt
foreign import ccall unsafe "LLVMParseBitcode" parseBitcode
    :: MemoryBufferRef -> (Ptr ModuleRef) -> (Ptr CString) -> IO CInt
foreign import ccall unsafe "LLVMGetBitcodeModuleProviderInContext" getBitcodeModuleProviderInContext
    :: ContextRef -> MemoryBufferRef -> (Ptr ModuleProviderRef) -> (Ptr CString) -> IO CInt
foreign import ccall unsafe "LLVMParseBitcodeInContext" parseBitcodeInContext
    :: ContextRef -> MemoryBufferRef -> (Ptr ModuleRef) -> (Ptr CString) -> IO CInt
foreign import ccall unsafe "LLVMGetBitcodeModule" getBitcodeModule
    :: MemoryBufferRef -> (Ptr ModuleRef) -> (Ptr CString) -> IO Bool
foreign import ccall unsafe "LLVMGetBitcodeModuleInContext" getBitcodeModuleInContext
    :: ContextRef -> MemoryBufferRef -> (Ptr ModuleRef) -> (Ptr CString) -> IO Bool
