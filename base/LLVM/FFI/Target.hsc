{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable #-}

module LLVM.FFI.Target where
import Data.Typeable
import Foreign.C.String (CString)
#if __GLASGOW_HASKELL__ >= 704
import Foreign.C.Types (CInt(..), CUInt(..), CULLong(..))
#else
import Foreign.C.Types (CInt, CUInt, CULLong)
#endif
import Foreign.Ptr (Ptr)

import LLVM.FFI.Core

-- enum { LLVMBigEndian, LLVMLittleEndian };
type ByteOrdering = CInt

data TargetData
    deriving (Typeable)
type TargetDataRef = Ptr TargetData

data TargetLibraryInfo
    deriving (Typeable)
type TargetLibraryInfoRef = Ptr TargetLibraryInfo

foreign import ccall unsafe "LLVMCreateTargetData" createTargetData
    :: CString -> IO TargetDataRef
foreign import ccall unsafe "LLVMAddTargetData" addTargetData
    :: TargetDataRef -> PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMAddTargetLibraryInfo" addTargetLibraryInfo
    :: TargetLibraryInfoRef -> PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMCopyStringRepOfTargetData" copyStringRepOfTargetData
    :: TargetDataRef -> IO CString
foreign import ccall unsafe "LLVMByteOrder" byteOrder
    :: TargetDataRef -> ByteOrdering
foreign import ccall unsafe "LLVMPointerSize" pointerSize
    :: TargetDataRef -> CUInt
foreign import ccall unsafe "LLVMIntPtrType" intPtrType
    :: TargetDataRef -> TypeRef
foreign import ccall unsafe "LLVMSizeOfTypeInBits" sizeOfTypeInBits
    :: TargetDataRef -> TypeRef -> CULLong
foreign import ccall unsafe "LLVMStoreSizeOfType" storeSizeOfType
    :: TargetDataRef -> TypeRef -> CULLong
foreign import ccall unsafe "LLVMABISizeOfType" aBISizeOfType
    :: TargetDataRef -> TypeRef -> CULLong
foreign import ccall unsafe "LLVMABIAlignmentOfType" aBIAlignmentOfType
    :: TargetDataRef -> TypeRef -> CULLong
foreign import ccall unsafe "LLVMCallFrameAlignmentOfType" callFrameAlignmentOfType
    :: TargetDataRef -> TypeRef -> CULLong
foreign import ccall unsafe "LLVMPreferredAlignmentOfType" preferredAlignmentOfType
    :: TargetDataRef -> TypeRef -> CULLong
foreign import ccall unsafe "LLVMPreferredAlignmentOfGlobal" preferredAlignmentOfGlobal
    :: TargetDataRef -> ValueRef -> CUInt
foreign import ccall unsafe "LLVMElementAtOffset" elementAtOffset
    :: TargetDataRef -> TypeRef -> CULLong -> CUInt
foreign import ccall unsafe "LLVMOffsetOfElement" offsetOfElement
    :: TargetDataRef -> TypeRef -> CUInt -> CULLong
foreign import ccall unsafe "LLVMDisposeTargetData" disposeTargetData
    :: TargetDataRef -> IO ()
