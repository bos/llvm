{-# LANGUAGE EmptyDataDecls #-}

module LLVM.Base
    (
      -- * Modules
      Module
    , ModuleRef
    , moduleCreateWithName
    , disposeModule

    -- * Module providers
    , ModuleProvider
    , ModuleProviderRef
    , createModuleProviderForExistingModule
    , disposeModuleProvider

    -- * Types
    , Type
    , TypeRef
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
    , ValueRef
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
    , BasicBlockRef
    , appendBasicBlock
    , insertBasicBlock
    , deleteBasicBlock
    , getEntryBasicBlock

    -- * Instruction building
    , Builder
    , BuilderRef
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
type ModuleRef = Ptr Module

foreign import ccall unsafe "LLVMModuleCreateWithName" moduleCreateWithName
    :: CString -> IO ModuleRef

foreign import ccall unsafe "LLVMDisposeModule" disposeModule
    :: ModuleRef -> IO ()


data ModuleProvider
type ModuleProviderRef = Ptr ModuleProvider

foreign import ccall unsafe "LLVMCreateModuleProviderForExistingModule"
    createModuleProviderForExistingModule
    :: ModuleRef -> IO ModuleProviderRef

foreign import ccall unsafe "LLVMDisposeModuleProvider" disposeModuleProvider
    :: ModuleProviderRef -> IO ()


data Type
type TypeRef = Ptr Type

foreign import ccall unsafe "LLVMInt1Type" int1Type :: IO TypeRef

foreign import ccall unsafe "LLVMInt8Type" int8Type :: IO TypeRef

foreign import ccall unsafe "LLVMInt16Type" int16Type :: IO TypeRef

foreign import ccall unsafe "LLVMInt32Type" int32Type :: IO TypeRef

foreign import ccall unsafe "LLVMInt64Type" int64Type :: IO TypeRef

foreign import ccall unsafe "LLVMIntType" integerType
    :: CUInt -> IO TypeRef

foreign import ccall unsafe "LLVMFloatType" floatType :: IO TypeRef

foreign import ccall unsafe "LLVMDoubleType" doubleType :: IO TypeRef

foreign import ccall unsafe "LLVMX86FP80Type" x86FP80Type :: IO TypeRef

foreign import ccall unsafe "LLVMFP128Type" fp128Type :: IO TypeRef

foreign import ccall unsafe "LLVMPPCFP128Type" ppcFP128Type :: IO TypeRef

foreign import ccall unsafe "LLVMFunctionType" functionType
        :: TypeRef -> Ptr TypeRef -> CUInt -> Int -> IO TypeRef

foreign import ccall unsafe "LLVMIsFunctionVarArg" isFunctionVarArg
        :: TypeRef -> IO CInt

foreign import ccall unsafe "LLVMGetReturnType" getReturnType
        :: TypeRef -> IO TypeRef

foreign import ccall unsafe "LLVMCountParamTypes" countParamTypes
        :: TypeRef -> IO CUInt

foreign import ccall unsafe "LLVMGetParamTypes" getParamTypes
        :: TypeRef -> Ptr TypeRef -> IO ()

foreign import ccall unsafe "LLVMPointerType" pointerType
    :: TypeRef -> CUInt -> IO TypeRef

foreign import ccall unsafe "LLVMAddTypeName" addTypeName
    :: ModuleRef -> CString -> TypeRef -> IO CInt

foreign import ccall unsafe "LLVMDeleteTypeName" deleteTypeName
    :: ModuleRef -> CString -> IO ()

foreign import ccall unsafe "LLVMGetElementType" getElementType
    :: TypeRef -> TypeRef


data Value
type ValueRef = Ptr Value

foreign import ccall unsafe "LLVMAddGlobal" addGlobal
    :: ModuleRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMDeleteGlobal" deleteGlobal
    :: ValueRef -> IO ()

foreign import ccall unsafe "LLVMSetInitializer" setInitializer
    :: ValueRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMTypeOf" typeOf
    :: ValueRef -> IO TypeRef

foreign import ccall unsafe "LLVMGetNamedFunction" getNamedFunction
    :: ModuleRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMAddFunction" addFunction
    :: ModuleRef -> CString -> TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMDeleteFunction" deleteFunction
    :: ValueRef -> IO ()

foreign import ccall unsafe "LLVMConstInt" constInt
    :: TypeRef -> CULLong -> CInt -> IO ValueRef

foreign import ccall unsafe "LLVMConstReal" constReal
    :: TypeRef -> CDouble -> IO ValueRef

foreign import ccall unsafe "LLVMConstString" constString
    :: CString -> CUInt -> CInt -> IO ValueRef

foreign import ccall unsafe "LLVMConstBitCast" constBitCast
    :: ValueRef -> TypeRef -> IO ValueRef


data BasicBlock
type BasicBlockRef = Ptr BasicBlock

foreign import ccall unsafe "LLVMAppendBasicBlock" appendBasicBlock
    :: ValueRef -> CString -> IO BasicBlockRef

foreign import ccall unsafe "LLVMInsertBasicBlock" insertBasicBlock
    :: BasicBlockRef -> CString -> IO BasicBlockRef

foreign import ccall unsafe "LLVMDeleteBasicBlock" deleteBasicBlock
    :: BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMGetEntryBasicBlock" getEntryBasicBlock
    :: ValueRef -> IO BasicBlockRef

data Builder
type BuilderRef = Ptr Builder

foreign import ccall unsafe "LLVMCreateBuilder" createBuilder
    :: IO BuilderRef

foreign import ccall unsafe "LLVMDisposeBuilder" disposeBuilder
    :: BuilderRef -> IO ()

foreign import ccall unsafe "LLVMPositionBuilderBefore" positionBefore
    :: BuilderRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMPositionBuilderAtEnd" positionAtEnd
    :: BuilderRef -> BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMBuildGEP" buildGEP
        :: BuilderRef -> ValueRef -> Ptr ValueRef -> CUInt -> CString
        -> IO ValueRef

foreign import ccall unsafe "LLVMBuildRet" buildRet
        :: BuilderRef -> ValueRef -> IO ValueRef           

foreign import ccall unsafe "LLVMBuildCall" buildCall
        :: BuilderRef -> ValueRef -> Ptr ValueRef -> CUInt -> CString
        -> IO ValueRef
