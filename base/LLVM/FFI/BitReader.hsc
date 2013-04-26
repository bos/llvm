{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

module LLVM.FFI.BitReader where
import Foreign.C.String(CString)
#if __GLASGOW_HASKELL__ >= 704
import Foreign.C.Types(CInt(..))
#else
import Foreign.C.Types(CInt)
#endif
import Foreign.Ptr(Ptr)

import LLVM.FFI.Core

foreign import ccall unsafe "LLVMGetBitcodeModuleProvider" getBitcodeModuleProvider
    :: MemoryBufferRef -> (Ptr ModuleProviderRef) -> (Ptr CString) -> IO Bool
foreign import ccall unsafe "LLVMParseBitcode" parseBitcode
    :: MemoryBufferRef -> (Ptr ModuleRef) -> (Ptr CString) -> IO Bool
foreign import ccall unsafe "LLVMGetBitcodeModuleProviderInContext" getBitcodeModuleProviderInContext
    :: ContextRef -> MemoryBufferRef -> (Ptr ModuleProviderRef) -> (Ptr CString) -> IO Bool
foreign import ccall unsafe "LLVMParseBitcodeInContext" parseBitcodeInContext
    :: ContextRef -> MemoryBufferRef -> (Ptr ModuleRef) -> (Ptr CString) -> IO Bool
foreign import ccall unsafe "LLVMGetBitcodeModule" getBitcodeModule
    :: MemoryBufferRef -> (Ptr ModuleRef) -> (Ptr CString) -> IO Bool
foreign import ccall unsafe "LLVMGetBitcodeModuleInContext" getBitcodeModuleInContext
    :: ContextRef -> MemoryBufferRef -> (Ptr ModuleRef) -> (Ptr CString) -> IO Bool
