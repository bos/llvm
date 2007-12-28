{-# LANGUAGE EmptyDataDecls #-}

module LLVM.Base
    (
      -- * Modules
      Module
    , moduleCreateWithName
    , disposeModule

    -- * Module providers
    , ModuleProvider
    , createModuleProviderForExistingModule
    , disposeModuleProvider

    -- * Types
    , Type
    , addTypeName
    , deleteTypeName
    , getElementType

    -- ** Integer types
    , int1Type
    , int8Type
    , int16Type
    , int32Type
    , int64Type
    , integerType

    -- ** Real types
    , floatType
    , doubleType
    , x86FP80Type
    , fp128Type
    , ppcFP128Type

    -- ** Function types
    , functionType
    , isFunctionVarArg
    , getReturnType
    , countParamTypes
    , getParamTypes

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
      
    -- * Constants

    -- ** Scalar constants
    , constInt
    , constReal

    -- ** Composite constants
    , constString

    -- ** Constant expressions
    , constBitCast

    -- * Basic blocks
    , BasicBlock
    , appendBasicBlock
    , insertBasicBlock
    , deleteBasicBlock
    , getEntryBasicBlock

    -- * Instruction building
    , Builder
    , createBuilder
    , disposeBuilder
    , positionBefore
    , positionAtEnd

    -- ** Memory
    , buildGEP

    -- ** Terminators
    , buildRet

    -- ** Miscellaneous instructions
    , buildCall
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


data ModuleProvider

foreign import ccall unsafe "LLVMCreateModuleProviderForExistingModule"
    createModuleProviderForExistingModule
    :: Ptr Module -> IO (Ptr ModuleProvider)

foreign import ccall unsafe "LLVMDisposeModuleProvider" disposeModuleProvider
    :: Ptr ModuleProvider -> IO ()


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

foreign import ccall unsafe "LLVMFunctionType" functionType
        :: Ptr Type -> Ptr (Ptr Type) -> CUInt -> Int -> IO (Ptr Type)

foreign import ccall unsafe "LLVMIsFunctionVarArg" isFunctionVarArg
        :: Ptr Type -> IO CInt

foreign import ccall unsafe "LLVMGetReturnType" getReturnType
        :: Ptr Type -> IO (Ptr Type)

foreign import ccall unsafe "LLVMCountParamTypes" countParamTypes
        :: Ptr Type -> IO CUInt

foreign import ccall unsafe "LLVMGetParamTypes" getParamTypes
        :: Ptr Type -> Ptr (Ptr Type) -> IO ()

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
    :: Ptr Module -> CString -> IO (Ptr Value)

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

foreign import ccall unsafe "LLVMAppendBasicBlock" appendBasicBlock
    :: Ptr Value -> CString -> IO (Ptr BasicBlock)

foreign import ccall unsafe "LLVMInsertBasicBlock" insertBasicBlock
    :: Ptr BasicBlock -> CString -> IO (Ptr BasicBlock)

foreign import ccall unsafe "LLVMDeleteBasicBlock" deleteBasicBlock
    :: Ptr BasicBlock -> IO ()

foreign import ccall unsafe "LLVMGetEntryBasicBlock" getEntryBasicBlock
    :: Ptr Value -> IO (Ptr BasicBlock)

data Builder

foreign import ccall unsafe "LLVMCreateBuilder" createBuilder
    :: IO (Ptr Builder)

foreign import ccall unsafe "LLVMDisposeBuilder" disposeBuilder
    :: Ptr Builder -> IO ()

foreign import ccall unsafe "LLVMPositionBuilderBefore" positionBefore
    :: Ptr Builder -> Ptr Value -> IO ()

foreign import ccall unsafe "LLVMPositionBuilderAtEnd" positionAtEnd
    :: Ptr Builder -> Ptr BasicBlock -> IO ()

foreign import ccall unsafe "LLVMBuildGEP" buildGEP
        :: Ptr Builder -> Ptr Value -> Ptr (Ptr Value) -> CUInt -> CString
        -> IO (Ptr Value)

foreign import ccall unsafe "LLVMBuildRet" buildRet
        :: Ptr Builder -> Ptr Value -> IO (Ptr Value)           

foreign import ccall unsafe "LLVMBuildCall" buildCall
        :: Ptr Builder -> Ptr Value -> Ptr (Ptr Value) -> CUInt -> CString
        -> IO (Ptr Value)
