{-# LANGUAGE EmptyDataDecls #-}

module LLVM.Base
    (
      Module
    , moduleCreateWithName
    , disposeModule

    , Type
    , addTypeName
    , deleteTypeName
    , getElementType

    , int1Type
    , int8Type
    , int16Type
    , int32Type
    , int64Type
    , integerType

    , floatType
    , doubleType
    , x86FP80Type
    , fp128Type
    , ppcFP128Type

    -- ** Array, pointer, and vector types
    , pointerType

    , Value
    , addGlobal
    , deleteGlobal
    , setInitializer
    , typeOf

    -- ** Functions
    , addFunction
    , deleteFunction
    , getNamedFunction
      
    -- ** Scalar constants
    , constInt
    , constReal

    -- ** Composite constants
    , constString

    -- ** Constant expressions
    , constBitCast

    -- ** Basic blocks
    , BasicBlock

    -- * Instruction building
    , Builder
    , createBuilder
    , disposeBuilder
    , positionBefore
    , positionAtEnd
    ) where

import Foreign.C.String (CString)
import Foreign.C.Types (CDouble, CInt, CUInt, CULLong)
import Foreign.Ptr (Ptr)

#include <llvm-c/Core.h>

data Module

foreign import ccall unsafe "LLVMModuleCreateWithName" moduleCreateWithName
    :: CString -> IO (Ptr Module)

foreign import ccall unsafe "LLVMDisposeModule" disposeModule
    :: Ptr Module -> IO ()


data Type

foreign import ccall unsafe "LLVMInt1Type" int1Type :: IO (Ptr Type)

foreign import ccall unsafe "LLVMInt8Type" int8Type :: IO (Ptr Type)

foreign import ccall unsafe "LLVMInt16Type" int16Type :: IO (Ptr Type)

foreign import ccall unsafe "LLVMInt32Type" int32Type :: IO (Ptr Type)

foreign import ccall unsafe "LLVMInt64Type" int64Type :: IO (Ptr Type)

foreign import ccall unsafe "LLVMIntType" integerType
    :: CUInt -> IO (Ptr Type)

foreign import ccall unsafe "LLVMFloatType" floatType :: IO (Ptr Type)

foreign import ccall unsafe "LLVMDoubleType" doubleType :: IO (Ptr Type)

foreign import ccall unsafe "LLVMX86FP80Type" x86FP80Type :: IO (Ptr Type)

foreign import ccall unsafe "LLVMFP128Type" fp128Type :: IO (Ptr Type)

foreign import ccall unsafe "LLVMPPCFP128Type" ppcFP128Type :: IO (Ptr Type)

foreign import ccall unsafe "LLVMPointerType" pointerType
    :: Ptr Type -> CUInt -> IO (Ptr Type)

foreign import ccall unsafe "LLVMAddTypeName" addTypeName
    :: Ptr Module -> CString -> Ptr Type -> IO CInt

foreign import ccall unsafe "LLVMDeleteTypeName" deleteTypeName
    :: Ptr Module -> CString -> IO ()

foreign import ccall unsafe "LLVMGetElementType" getElementType
    :: Ptr Type -> IO (Ptr Type)


data Value

foreign import ccall unsafe "LLVMAddGlobal" addGlobal
    :: Ptr Module -> Ptr Type -> CString -> IO (Ptr Value)

foreign import ccall unsafe "LLVMDeleteGlobal" deleteGlobal
    :: Ptr Value -> IO ()

foreign import ccall unsafe "LLVMSetInitializer" setInitializer
    :: Ptr Value -> Ptr Value -> IO ()

foreign import ccall unsafe "LLVMTypeOf" typeOf
    :: Ptr Value -> IO (Ptr Type)

foreign import ccall unsafe "LLVMGetNamedFunction" getNamedFunction
    :: CString -> IO (Ptr Value)

foreign import ccall unsafe "LLVMAddFunction" addFunction
    :: Ptr Module -> CString -> Ptr Type -> IO (Ptr Value)

foreign import ccall unsafe "LLVMDeleteFunction" deleteFunction
    :: Ptr Value -> IO ()

foreign import ccall unsafe "LLVMConstInt" constInt
    :: Ptr Type -> CULLong -> CInt -> IO (Ptr Value)

foreign import ccall unsafe "LLVMConstReal" constReal
    :: Ptr Type -> CDouble -> IO (Ptr Value)

foreign import ccall unsafe "LLVMConstString" constString
    :: CString -> CUInt -> CInt -> IO (Ptr Value)

foreign import ccall unsafe "LLVMConstBitCast" constBitCast
    :: Ptr Value -> Ptr Type -> IO (Ptr Value)


data BasicBlock


data Builder

foreign import ccall unsafe "LLVMCreateBuilder" createBuilder
    :: IO (Ptr Builder)

foreign import ccall unsafe "LLVMDisposeBuilder" disposeBuilder
    :: Ptr Builder -> IO ()

foreign import ccall unsafe "LLVMPositionBuilderBefore" positionBefore
    :: Ptr Builder -> Ptr Value -> IO ()

foreign import ccall unsafe "LLVMPositionBuilderAtEnd" positionAtEnd
    :: Ptr Builder -> Ptr BasicBlock -> IO ()
