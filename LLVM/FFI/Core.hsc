{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

-- |
-- Module:      LLVM.FFI.Core
-- Copyright:   Bryan O'Sullivan 2007, 2008
-- License:     BSD-style (see the file LICENSE)
--
-- Maintainer:  bos@serpentine.com
-- Stability:   experimental
-- Portability: requires GHC 6.8, LLVM
--
-- This module provides direct access to the LLVM C bindings.

module LLVM.FFI.Core
    (
      -- * Modules
      Module
    , ModuleRef
    , moduleCreateWithName
    , disposeModule

    , getDataLayout
    , setDataLayout

    , getTarget
    , setTarget

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

    , getTypeKind
    , TypeKind(..)

    -- ** Integer types
    , int1Type
    , int8Type
    , int16Type
    , int32Type
    , int64Type
    , integerType
    , getIntTypeWidth

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

    -- ** Other types
    , voidType
    , labelType
    , opaqueType

    -- ** Array, pointer, and vector types
    , arrayType
    , pointerType
    , vectorType
    , getElementType
    , getArrayLength
    , getPointerAddressSpace
    , getVectorSize

    -- ** Struct types
    , structType
    , countStructElementTypes
    , getStructElementTypes
    , isPackedStruct

    -- * Type handles
    , createTypeHandle
    , refineType
    , resolveTypeHandle
    , disposeTypeHandle

    -- * Values
    , Value
    , ValueRef
    , typeOf
    , getValueName
    , setValueName
    , dumpValue

    -- ** Constants
    , constNull
    , constAllOnes
    , getUndef
    , isConstant
    , isNull
    , isUndef

    -- ** Global variables, functions, and aliases (globals)
    , Linkage
    , Visibility
    , isDeclaration
    , getLinkage
    , setLinkage
    , getSection
    , setSection
    , getVisibility
    , setVisibility
    , getAlignment
    , setAlignment
      
    -- ** Global variables
    , addGlobal
    , getNamedGlobal
    , deleteGlobal
    , getInitializer
    , setInitializer
    , isThreadLocal
    , setThreadLocal
    , isGlobalConstant
    , setGlobalConstant
    , getFirstGlobal
    , getNextGlobal
    , getPreviousGlobal
    , getLastGlobal
    , getGlobalParent

    -- ** Functions
    , addFunction
    , getNamedFunction
    , deleteFunction
    , countParams
    , getParams
    , getParam
    , getIntrinsicID
    , getGC
    , setGC
    , getFirstFunction
    , getNextFunction
    , getPreviousFunction
    , getLastFunction
    , getFirstParam
    , getNextParam
    , getPreviousParam
    , getLastParam
    , getParamParent
    , isTailCall
    , setTailCall

    -- ** Phi nodes
    , addIncoming
    , countIncoming
    , getIncomingValue
    , getIncomingBlock

    -- ** Calling conventions
    , CallingConvention(..)
    , fromCallingConvention
    , toCallingConvention
    , getFunctionCallConv
    , setFunctionCallConv
    , getInstructionCallConv
    , setInstructionCallConv

    -- * Constants

    -- ** Scalar constants
    , constInt
    , constReal

    -- ** Composite constants
    , constArray
    , constString
    , constStruct
    , constVector

    -- ** Constant expressions
    , sizeOf
    , constNeg
    , constNot
    , constAdd
    , constSub
    , constMul
    , constUDiv
    , constSDiv
    , constFDiv
    , constURem
    , constSRem
    , constFRem
    , constAnd
    , constOr
    , constXor
    , constICmp
    , constFCmp
    , constShl
    , constLShr
    , constAShr
    , constGEP
    , constTrunc
    , constSExt
    , constZExt
    , constFPTrunc
    , constFPExt
    , constUIToFP
    , constSIToFP
    , constFPToUI
    , constFPToSI
    , constPtrToInt
    , constIntToPtr
    , constBitCast
    , constSelect
    , constExtractElement
    , constInsertElement
    , constShuffleVector
    , constRealOfString

    -- * Basic blocks
    , BasicBlock
    , BasicBlockRef
    , basicBlockAsValue
    , valueIsBasicBlock
    , valueAsBasicBlock
    , countBasicBlocks
    , getBasicBlocks
    , getEntryBasicBlock
    , appendBasicBlock
    , insertBasicBlock
    , deleteBasicBlock
    , getFirstBasicBlock
    , getNextBasicBlock
    , getPreviousBasicBlock
    , getLastBasicBlock
    , getInsertBlock
    , getBasicBlockParent

    -- * Instruction building
    , Builder
    , BuilderRef
    , createBuilder
    , disposeBuilder
    , positionBuilder
    , positionBefore
    , positionAtEnd
    , getFirstInstruction
    , getNextInstruction
    , getPreviousInstruction
    , getLastInstruction
    , getInstructionParent

    -- ** Terminators
    , buildRetVoid
    , buildRet
    , buildBr
    , buildCondBr
    , buildSwitch
    , buildInvoke
    , buildUnwind
    , buildUnreachable

    -- ** Arithmetic
    , buildAdd
    , buildSub
    , buildMul
    , buildUDiv
    , buildSDiv
    , buildFDiv
    , buildURem
    , buildSRem
    , buildFRem
    , buildShl
    , buildLShr
    , buildAShr
    , buildAnd
    , buildOr
    , buildXor
    , buildNeg
    , buildNot

    -- ** Memory
    , buildMalloc
    , buildArrayMalloc
    , buildAlloca
    , buildArrayAlloca
    , buildFree
    , buildLoad
    , buildStore
    , buildGEP

    -- ** Casts
    , buildTrunc
    , buildZExt
    , buildSExt
    , buildFPToUI
    , buildFPToSI
    , buildUIToFP
    , buildSIToFP
    , buildFPTrunc
    , buildFPExt
    , buildPtrToInt
    , buildIntToPtr
    , buildBitCast

    -- ** Comparisons
    , buildICmp
    , buildFCmp

    -- ** Miscellaneous instructions
    , buildPhi
    , buildCall
    , buildSelect
    , buildVAArg
    , buildExtractElement
    , buildInsertElement
    , buildShuffleVector

    -- ** Other helpers
    , addCase

    -- * Memory buffers
    , MemoryBuffer
    , MemoryBufferRef
    , createMemoryBufferWithContentsOfFile
    , createMemoryBufferWithSTDIN
    , disposeMemoryBuffer

    -- * Error handling
    , disposeMessage

    -- * Parameter passing
    , addInstrAttribute
    , addAttribute
    , removeInstrAttribute
    , removeAttribute
    , setInstrParamAlignment
    , setParamAlignment

    -- * Pass manager
    , PassManager
    , PassManagerRef
    , createFunctionPassManager
    , createPassManager
    , disposePassManager
    , finalizeFunctionPassManager
    , initializeFunctionPassManager
    , runFunctionPassManager
    , runPassManager

    , dumpModule
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

foreign import ccall unsafe "LLVMGetDataLayout" getDataLayout
    :: ModuleRef -> IO CString

foreign import ccall unsafe "LLVMSetDataLayout" setDataLayout
    :: ModuleRef -> CString -> IO ()


data ModuleProvider
type ModuleProviderRef = Ptr ModuleProvider

foreign import ccall unsafe "LLVMCreateModuleProviderForExistingModule"
    createModuleProviderForExistingModule
    :: ModuleRef -> IO ModuleProviderRef

foreign import ccall unsafe "LLVMDisposeModuleProvider" disposeModuleProvider
    :: ModuleProviderRef -> IO ()


data Type
type TypeRef = Ptr Type

foreign import ccall unsafe "LLVMInt1Type" int1Type :: TypeRef

foreign import ccall unsafe "LLVMInt8Type" int8Type :: TypeRef

foreign import ccall unsafe "LLVMInt16Type" int16Type :: TypeRef

foreign import ccall unsafe "LLVMInt32Type" int32Type :: TypeRef

foreign import ccall unsafe "LLVMInt64Type" int64Type :: TypeRef

-- | An integer type of the given width.
foreign import ccall unsafe "LLVMIntType" integerType
    :: CUInt                    -- ^ width in bits
    -> TypeRef

foreign import ccall unsafe "LLVMFloatType" floatType :: TypeRef

foreign import ccall unsafe "LLVMDoubleType" doubleType :: TypeRef

foreign import ccall unsafe "LLVMX86FP80Type" x86FP80Type :: TypeRef

foreign import ccall unsafe "LLVMFP128Type" fp128Type :: TypeRef

foreign import ccall unsafe "LLVMPPCFP128Type" ppcFP128Type :: TypeRef

foreign import ccall unsafe "LLVMVoidType" voidType :: TypeRef

-- | Create a function type.
foreign import ccall unsafe "LLVMFunctionType" functionType
        :: TypeRef              -- ^ return type
        -> Ptr TypeRef          -- ^ array of argument types
        -> CUInt                -- ^ number of elements in array
        -> CInt                 -- ^ non-zero if function is varargs
        -> TypeRef

-- | Indicate whether a function takes varargs.
foreign import ccall unsafe "LLVMIsFunctionVarArg" isFunctionVarArg
        :: TypeRef -> IO CInt

-- | Give a function's return type.
foreign import ccall unsafe "LLVMGetReturnType" getReturnType
        :: TypeRef -> IO TypeRef

-- | Give the number of fixed parameters that a function takes.
foreign import ccall unsafe "LLVMCountParamTypes" countParamTypes
        :: TypeRef -> IO CUInt

-- | Fill out an array with the types of a function's fixed
-- parameters.
foreign import ccall unsafe "LLVMGetParamTypes" getParamTypes
        :: TypeRef -> Ptr TypeRef -> IO ()

foreign import ccall unsafe "LLVMArrayType" arrayType
    :: TypeRef                  -- ^ element type
    -> CUInt                    -- ^ element count
    -> TypeRef

foreign import ccall unsafe "LLVMPointerType" pointerType
    :: TypeRef                  -- ^ pointed-to type
    -> CUInt                    -- ^ address space
    -> TypeRef

foreign import ccall unsafe "LLVMVectorType" vectorType
    :: TypeRef                  -- ^ element type
    -> CUInt                    -- ^ element count
    -> TypeRef

foreign import ccall unsafe "LLVMAddTypeName" addTypeName
    :: ModuleRef -> CString -> TypeRef -> IO CInt

foreign import ccall unsafe "LLVMDeleteTypeName" deleteTypeName
    :: ModuleRef -> CString -> IO ()

-- | Get the type of a sequential type's elements.
foreign import ccall unsafe "LLVMGetElementType" getElementType
    :: TypeRef -> IO TypeRef


data Value
type ValueRef = Ptr Value

foreign import ccall unsafe "LLVMAddGlobal" addGlobal
    :: ModuleRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMDeleteGlobal" deleteGlobal
    :: ValueRef -> IO ()

foreign import ccall unsafe "LLVMSetInitializer" setInitializer
    :: ValueRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMGetNamedGlobal" getNamedGlobal
    :: ModuleRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMGetInitializer" getInitializer
    :: ValueRef -> IO ValueRef

foreign import ccall unsafe "LLVMIsThreadLocal" isThreadLocal
    :: ValueRef -> IO CInt

foreign import ccall unsafe "LLVMSetThreadLocal" setThreadLocal
    :: ValueRef -> CInt -> IO ()

foreign import ccall unsafe "LLVMIsGlobalConstant" isGlobalConstant
    :: ValueRef -> IO CInt

foreign import ccall unsafe "LLVMSetGlobalConstant" setGlobalConstant
    :: ValueRef -> CInt -> IO ()

foreign import ccall unsafe "LLVMTypeOf" typeOf
    :: ValueRef -> IO TypeRef

foreign import ccall unsafe "LLVMGetValueName" getValueName
    :: ValueRef -> IO CString

foreign import ccall unsafe "LLVMSetValueName" setValueName
    :: ValueRef -> CString -> IO ()

foreign import ccall unsafe "LLVMDumpValue" dumpValue
    :: ValueRef -> IO ()

foreign import ccall unsafe "LLVMConstAllOnes" constAllOnes
    :: TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstArray" constArray
    :: TypeRef -> Ptr ValueRef -> CUInt -> ValueRef

foreign import ccall unsafe "LLVMConstNull" constNull
    :: TypeRef -> ValueRef

foreign import ccall unsafe "LLVMIsConstant" isConstant
    :: ValueRef -> IO CInt

foreign import ccall unsafe "LLVMGetUndef" getUndef
    :: TypeRef -> ValueRef

foreign import ccall unsafe "LLVMIsNull" isNull
    :: ValueRef -> IO CInt

foreign import ccall unsafe "LLVMIsUndef" isUndef
    :: ValueRef -> IO CInt

foreign import ccall unsafe "LLVMGetNamedFunction" getNamedFunction
    :: ModuleRef                -- ^ module
    -> CString                  -- ^ name
    -> IO ValueRef              -- ^ function (@nullPtr@ if not found)

foreign import ccall unsafe "LLVMAddFunction" addFunction
    :: ModuleRef                -- ^ module
    -> CString                  -- ^ name
    -> TypeRef                  -- ^ type
    -> IO ValueRef

foreign import ccall unsafe "LLVMDeleteFunction" deleteFunction
    :: ValueRef                 -- ^ function
    -> IO ()

foreign import ccall unsafe "LLVMCountParams" countParams
    :: ValueRef                 -- ^ function
    -> CUInt

foreign import ccall unsafe "LLVMGetParam" getParam
    :: ValueRef                 -- ^ function
    -> CUInt                    -- ^ offset into array
    -> ValueRef

foreign import ccall unsafe "LLVMGetParams" getParams
    :: ValueRef                 -- ^ function
    -> Ptr ValueRef             -- ^ array to fill out
    -> IO ()

foreign import ccall unsafe "LLVMGetIntrinsicID" getIntrinsicID
    :: ValueRef                 -- ^ function
    -> CUInt

data CallingConvention = C
                       | Fast
                       | Cold
                       | X86StdCall
                       | X86FastCall
                         deriving (Eq, Show)

fromCallingConvention :: CallingConvention -> CUInt
fromCallingConvention C = (#const LLVMCCallConv)
fromCallingConvention Fast = (#const LLVMFastCallConv)
fromCallingConvention Cold = (#const LLVMColdCallConv)
fromCallingConvention X86StdCall = (#const LLVMX86FastcallCallConv)
fromCallingConvention X86FastCall = (#const LLVMX86StdcallCallConv)

toCallingConvention :: CUInt -> CallingConvention
toCallingConvention c | c == (#const LLVMCCallConv) = C
toCallingConvention c | c == (#const LLVMFastCallConv) = Fast
toCallingConvention c | c == (#const LLVMColdCallConv) = Cold
toCallingConvention c | c == (#const LLVMX86StdcallCallConv) = X86StdCall
toCallingConvention c | c == (#const LLVMX86FastcallCallConv) = X86FastCall
toCallingConvention c = error $ "LLVM.Core.FFI.toCallingConvention: " ++
                                "unsupported calling convention" ++ show c

foreign import ccall unsafe "LLVMGetFunctionCallConv" getFunctionCallConv
    :: ValueRef                 -- ^ function
    -> IO CUInt

foreign import ccall unsafe "LLVMSetFunctionCallConv" setFunctionCallConv
    :: ValueRef                 -- ^ function
    -> CUInt
    -> IO ()

foreign import ccall unsafe "LLVMGetGC" getGC
    :: ValueRef -> IO CString

foreign import ccall unsafe "LLVMSetGC" setGC
    :: ValueRef -> CString -> IO ()

foreign import ccall unsafe "LLVMIsDeclaration" isDeclaration
    :: ValueRef -> IO CInt

type Linkage = CUInt

foreign import ccall unsafe "LLVMGetLinkage" getLinkage
    :: ValueRef -> IO Linkage

foreign import ccall unsafe "LLVMSetLinkage" setLinkage
    :: ValueRef -> Linkage -> IO ()

foreign import ccall unsafe "LLVMGetSection" getSection
    :: ValueRef -> IO CString

foreign import ccall unsafe "LLVMSetSection" setSection
    :: ValueRef -> CString -> IO ()

type Visibility = CUInt

foreign import ccall unsafe "LLVMGetVisibility" getVisibility
    :: ValueRef -> IO Visibility

foreign import ccall unsafe "LLVMSetVisibility" setVisibility
    :: ValueRef -> Visibility -> IO ()

foreign import ccall unsafe "LLVMGetAlignment" getAlignment
    :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMSetAlignment" setAlignment
    :: ValueRef -> CUInt -> IO ()


foreign import ccall unsafe "LLVMConstInt" constInt
    :: TypeRef -> CULLong -> CInt -> ValueRef

foreign import ccall unsafe "LLVMConstReal" constReal
    :: TypeRef -> CDouble -> ValueRef

foreign import ccall unsafe "LLVMConstString" constString
    :: CString -> CUInt -> CInt -> ValueRef

foreign import ccall unsafe "LLVMConstStruct" constStruct
    :: Ptr ValueRef -> CUInt -> CInt -> ValueRef

foreign import ccall unsafe "LLVMConstVector" constVector
    :: Ptr ValueRef -> CUInt -> ValueRef

foreign import ccall unsafe "LLVMConstNeg" constNeg
    :: ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstNot" constNot
    :: ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstAdd" constAdd
    :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstSub" constSub
    :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstMul" constMul
    :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstUDiv" constUDiv
    :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstSDiv" constSDiv
    :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstFDiv" constFDiv
    :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstURem" constURem
    :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstSRem" constSRem
    :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstFRem" constFRem
    :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstAnd" constAnd
    :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstOr" constOr
    :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstXor" constXor
    :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstICmp" constICmp
    :: CInt -> ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstFCmp" constFCmp
    :: CInt -> ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstShl" constShl
    :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstLShr" constLShr
    :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstAShr" constAShr
    :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstGEP" constGEP
    :: ValueRef -> Ptr ValueRef -> CUInt -> ValueRef

foreign import ccall unsafe "LLVMConstTrunc" constTrunc
    :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstSExt" constSExt
    :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstZExt" constZExt
    :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstFPTrunc" constFPTrunc
    :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstFPExt" constFPExt
    :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstUIToFP" constUIToFP
    :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstSIToFP" constSIToFP
    :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstFPToUI" constFPToUI
    :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstFPToSI" constFPToSI
    :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstPtrToInt" constPtrToInt
    :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstIntToPtr" constIntToPtr
    :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstBitCast" constBitCast
    :: ValueRef -> TypeRef -> ValueRef

foreign import ccall unsafe "LLVMConstSelect" constSelect
    :: ValueRef -> ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstExtractElement" constExtractElement
    :: ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstInsertElement" constInsertElement
    :: ValueRef -> ValueRef -> ValueRef -> ValueRef

foreign import ccall unsafe "LLVMConstShuffleVector" constShuffleVector
    :: ValueRef -> ValueRef -> ValueRef -> ValueRef

type BasicBlock = Value
type BasicBlockRef = Ptr BasicBlock

foreign import ccall unsafe "LLVMBasicBlockAsValue" basicBlockAsValue
    :: BasicBlockRef -> ValueRef

foreign import ccall unsafe "LLVMValueIsBasicBlock" valueIsBasicBlock
    :: ValueRef -> Bool

foreign import ccall unsafe "LLVMValueAsBasicBlock" valueAsBasicBlock
    :: ValueRef                 -- ^ basic block
    -> BasicBlockRef

foreign import ccall unsafe "LLVMCountBasicBlocks" countBasicBlocks
    :: ValueRef                 -- ^ function
    -> IO CUInt

foreign import ccall unsafe "LLVMGetBasicBlocks" getBasicBlocks
    :: ValueRef                 -- ^ function
    -> Ptr BasicBlockRef        -- ^ array to fill out
    -> IO ()

foreign import ccall unsafe "LLVMGetEntryBasicBlock" getEntryBasicBlock
    :: ValueRef                 -- ^ function
    -> IO BasicBlockRef

foreign import ccall unsafe "LLVMAppendBasicBlock" appendBasicBlock
    :: ValueRef                 -- ^ function
    -> CString                  -- ^ name for label
    -> IO BasicBlockRef

foreign import ccall unsafe "LLVMInsertBasicBlock" insertBasicBlock
    :: BasicBlockRef            -- ^ insert before this one
    -> CString                  -- ^ name for label
    -> IO BasicBlockRef

foreign import ccall unsafe "LLVMDeleteBasicBlock" deleteBasicBlock
    :: BasicBlockRef -> IO ()

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

foreign import ccall unsafe "LLVMBuildRetVoid" buildRetVoid
    :: BuilderRef -> IO ValueRef
foreign import ccall unsafe "LLVMBuildRet" buildRet
    :: BuilderRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMBuildBr" buildBr
    :: BuilderRef -> BasicBlockRef -> IO ValueRef
foreign import ccall unsafe "LLVMBuildCondBr" buildCondBr
    :: BuilderRef -> ValueRef -> BasicBlockRef -> BasicBlockRef -> IO ValueRef
foreign import ccall unsafe "LLVMBuildSwitch" buildSwitch
    :: BuilderRef -> ValueRef -> BasicBlockRef -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMBuildInvoke" buildInvoke
    :: BuilderRef -> ValueRef -> Ptr ValueRef -> CUInt
    -> BasicBlockRef -> BasicBlockRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildUnwind" buildUnwind
    :: BuilderRef -> IO ValueRef
foreign import ccall unsafe "LLVMBuildUnreachable" buildUnreachable
    :: BuilderRef -> IO ValueRef

foreign import ccall unsafe "LLVMBuildAdd" buildAdd
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildSub" buildSub
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildMul" buildMul
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildUDiv" buildUDiv
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildSDiv" buildSDiv
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFDiv" buildFDiv
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildURem" buildURem
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildSRem" buildSRem
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFRem" buildFRem
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildShl" buildShl
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildLShr" buildLShr
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildAShr" buildAShr
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildAnd" buildAnd
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildOr" buildOr
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildXor" buildXor
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNeg" buildNeg
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNot" buildNot
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef

-- Memory
foreign import ccall unsafe "LLVMBuildMalloc" buildMalloc
    :: BuilderRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildArrayMalloc" buildArrayMalloc
    :: BuilderRef -> TypeRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildAlloca" buildAlloca
    :: BuilderRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildArrayAlloca" buildArrayAlloca
    :: BuilderRef -> TypeRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFree" buildFree
    :: BuilderRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMBuildLoad" buildLoad
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildStore" buildStore
    :: BuilderRef -> ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMBuildGEP" buildGEP
    :: BuilderRef -> ValueRef -> Ptr ValueRef -> CUInt -> CString
    -> IO ValueRef

-- Casts
foreign import ccall unsafe "LLVMBuildTrunc" buildTrunc
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildZExt" buildZExt
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildSExt" buildSExt
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFPToUI" buildFPToUI
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFPToSI" buildFPToSI
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildUIToFP" buildUIToFP
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildSIToFP" buildSIToFP
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFPTrunc" buildFPTrunc
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFPExt" buildFPExt
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildPtrToInt" buildPtrToInt
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildIntToPtr" buildIntToPtr
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildBitCast" buildBitCast
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

-- Comparisons
foreign import ccall unsafe "LLVMBuildICmp" buildICmp
    :: BuilderRef -> CInt -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFCmp" buildFCmp
    :: BuilderRef -> CInt -> ValueRef -> ValueRef -> CString -> IO ValueRef

-- Miscellaneous instructions
foreign import ccall unsafe "LLVMBuildPhi" buildPhi
    :: BuilderRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildCall" buildCall
    :: BuilderRef -> ValueRef -> Ptr ValueRef -> CUInt -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildSelect" buildSelect
    :: BuilderRef -> ValueRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildVAArg" buildVAArg
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildExtractElement" buildExtractElement
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildInsertElement" buildInsertElement
    :: BuilderRef -> ValueRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildShuffleVector" buildShuffleVector
    :: BuilderRef -> ValueRef -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMAddCase" addCase
    :: ValueRef -> ValueRef -> BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMCountIncoming" countIncoming
    :: ValueRef -> IO CUInt
foreign import ccall unsafe "LLVMAddIncoming" addIncoming
    :: ValueRef -> Ptr ValueRef -> Ptr ValueRef -> CUInt -> IO ()
foreign import ccall unsafe "LLVMGetIncomingValue" getIncomingValue
    :: ValueRef -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMGetIncomingBlock" getIncomingBlock
    :: ValueRef -> CUInt -> IO BasicBlockRef
       
foreign import ccall unsafe "LLVMGetInstructionCallConv" getInstructionCallConv
    :: ValueRef -> IO CUInt
foreign import ccall unsafe "LLVMSetInstructionCallConv" setInstructionCallConv
    :: ValueRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMStructType" structType
    :: (Ptr TypeRef) -> CUInt -> CInt -> IO TypeRef
foreign import ccall unsafe "LLVMCountStructElementTypes"
    countStructElementTypes :: TypeRef -> IO CUInt
foreign import ccall unsafe "LLVMGetStructElementTypes" getStructElementTypes
    :: TypeRef -> (Ptr TypeRef) -> IO ()
foreign import ccall unsafe "LLVMIsPackedStruct" isPackedStruct
    :: TypeRef -> IO CInt

data MemoryBuffer
type MemoryBufferRef = Ptr MemoryBuffer

data TypeHandle
type TypeHandleRef = Ptr TypeHandle

data TypeKind
    = VoidTypeKind
    | FloatTypeKind
    | DoubleTypeKind
    | X86_FP80TypeKind
    | FP128TypeKind
    | PPC_FP128TypeKind
    | LabelTypeKind
    | IntegerTypeKind
    | FunctionTypeKind
    | StructTypeKind
    | ArrayTypeKind
    | PointerTypeKind
    | OpaqueTypeKind
    | VectorTypeKind
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

getTypeKind :: TypeRef -> IO TypeKind
getTypeKind = fmap (toEnum . fromIntegral) . getTypeKindCUInt

foreign import ccall unsafe "LLVMCreateMemoryBufferWithContentsOfFile" createMemoryBufferWithContentsOfFile
    :: CString -> Ptr MemoryBufferRef -> Ptr CString -> IO CInt
foreign import ccall unsafe "LLVMCreateMemoryBufferWithSTDIN" createMemoryBufferWithSTDIN
    :: Ptr MemoryBufferRef -> Ptr CString -> IO CInt
foreign import ccall unsafe "LLVMCreateTypeHandle" createTypeHandle
    :: TypeRef -> IO TypeHandleRef
foreign import ccall unsafe "LLVMDisposeMemoryBuffer" disposeMemoryBuffer
    :: MemoryBufferRef -> IO ()
foreign import ccall unsafe "LLVMDisposeMessage" disposeMessage
    :: CString -> IO ()
foreign import ccall unsafe "LLVMDisposeTypeHandle" disposeTypeHandle
    :: TypeHandleRef -> IO ()
foreign import ccall unsafe "LLVMGetArrayLength" getArrayLength
    :: TypeRef -> IO CUInt
foreign import ccall unsafe "LLVMGetIntTypeWidth" getIntTypeWidth
    :: TypeRef -> IO CUInt
foreign import ccall unsafe "LLVMGetPointerAddressSpace" getPointerAddressSpace
    :: TypeRef -> IO CUInt
foreign import ccall unsafe "LLVMGetTarget" getTarget
    :: ModuleRef -> IO CString
foreign import ccall unsafe "LLVMGetTypeKind" getTypeKindCUInt
    :: TypeRef -> IO CUInt
foreign import ccall unsafe "LLVMGetVectorSize" getVectorSize
    :: TypeRef -> IO CUInt
foreign import ccall unsafe "LLVMRefineType" refineType
    :: TypeRef -> TypeRef -> IO ()
foreign import ccall unsafe "LLVMResolveTypeHandle" resolveTypeHandle
    :: TypeHandleRef -> IO TypeRef
foreign import ccall unsafe "LLVMSetTarget" setTarget
    :: ModuleRef -> CString -> IO ()
foreign import ccall unsafe "LLVMSizeOf" sizeOf
    :: TypeRef -> IO ValueRef

{-
typedef enum {
    LLVMZExtAttribute       = 1<<0,
    LLVMSExtAttribute       = 1<<1,
    LLVMNoReturnAttribute   = 1<<2,
    LLVMInRegAttribute      = 1<<3,
    LLVMStructRetAttribute  = 1<<4,
    LLVMNoUnwindAttribute   = 1<<5,
    LLVMNoAliasAttribute    = 1<<6,
    LLVMByValAttribute      = 1<<7,
    LLVMNestAttribute       = 1<<8,
    LLVMReadNoneAttribute   = 1<<9,
    LLVMReadOnlyAttribute   = 1<<10
} LLVMAttribute;
-}
type Attribute = CInt

data PassManager
type PassManagerRef = Ptr PassManager

foreign import ccall unsafe "LLVMConstRealOfString" constRealOfString
    :: TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMCreateFunctionPassManager" createFunctionPassManager
    :: ModuleProviderRef -> IO PassManagerRef
foreign import ccall unsafe "LLVMCreatePassManager" createPassManager
    :: IO PassManagerRef
foreign import ccall unsafe "LLVMDisposePassManager" disposePassManager
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "LLVMDumpModule" dumpModule
    :: ModuleRef -> IO ()
foreign import ccall unsafe "LLVMFinalizeFunctionPassManager" finalizeFunctionPassManager
    :: PassManagerRef -> IO CInt
foreign import ccall unsafe "LLVMGetBasicBlockParent" getBasicBlockParent
    :: BasicBlockRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetFirstBasicBlock" getFirstBasicBlock
    :: ValueRef -> IO BasicBlockRef
foreign import ccall unsafe "LLVMGetFirstFunction" getFirstFunction
    :: ModuleRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetFirstGlobal" getFirstGlobal
    :: ModuleRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetFirstInstruction" getFirstInstruction
    :: BasicBlockRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetFirstParam" getFirstParam
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetGlobalParent" getGlobalParent
    :: ValueRef -> IO ModuleRef
foreign import ccall unsafe "LLVMGetInsertBlock" getInsertBlock
    :: BuilderRef -> IO BasicBlockRef
foreign import ccall unsafe "LLVMGetInstructionParent" getInstructionParent
    :: ValueRef -> IO BasicBlockRef
foreign import ccall unsafe "LLVMGetLastBasicBlock" getLastBasicBlock
    :: ValueRef -> IO BasicBlockRef
foreign import ccall unsafe "LLVMGetLastFunction" getLastFunction
    :: ModuleRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetLastGlobal" getLastGlobal
    :: ModuleRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetLastInstruction" getLastInstruction
    :: BasicBlockRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetLastParam" getLastParam
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetNextBasicBlock" getNextBasicBlock
    :: BasicBlockRef -> IO BasicBlockRef
foreign import ccall unsafe "LLVMGetNextFunction" getNextFunction
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetNextGlobal" getNextGlobal
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetNextInstruction" getNextInstruction
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetNextParam" getNextParam
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetParamParent" getParamParent
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetPreviousBasicBlock" getPreviousBasicBlock
    :: BasicBlockRef -> IO BasicBlockRef
foreign import ccall unsafe "LLVMGetPreviousFunction" getPreviousFunction
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetPreviousGlobal" getPreviousGlobal
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetPreviousInstruction" getPreviousInstruction
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetPreviousParam" getPreviousParam
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMInitializeFunctionPassManager" initializeFunctionPassManager
    :: PassManagerRef -> IO CInt
foreign import ccall unsafe "LLVMLabelType" labelType
    :: IO TypeRef
foreign import ccall unsafe "LLVMOpaqueType" opaqueType
    :: IO TypeRef
foreign import ccall unsafe "LLVMPositionBuilder" positionBuilder
    :: BuilderRef -> BasicBlockRef -> ValueRef -> IO ()
foreign import ccall unsafe "LLVMRunFunctionPassManager" runFunctionPassManager
    :: PassManagerRef -> ValueRef -> IO CInt
foreign import ccall unsafe "LLVMRunPassManager" runPassManager
    :: PassManagerRef -> ModuleRef -> IO CInt
foreign import ccall unsafe "LLVMSetInstrParamAlignment" setInstrParamAlignment
    :: ValueRef -> CUInt -> CUInt -> IO ()
foreign import ccall unsafe "LLVMSetParamAlignment" setParamAlignment
    :: ValueRef -> CUInt -> IO ()
foreign import ccall unsafe "LLVMAddAttribute" addAttribute
    :: ValueRef -> Attribute -> IO ()
foreign import ccall unsafe "LLVMAddInstrAttribute" addInstrAttribute
    :: ValueRef -> CUInt -> Attribute -> IO ()
foreign import ccall unsafe "LLVMIsTailCall" isTailCall
    :: ValueRef -> IO CInt
foreign import ccall unsafe "LLVMRemoveAttribute" removeAttribute
    :: ValueRef -> Attribute -> IO ()
foreign import ccall unsafe "LLVMRemoveInstrAttribute" removeInstrAttribute
    :: ValueRef -> CUInt -> Attribute -> IO ()
foreign import ccall unsafe "LLVMSetTailCall" setTailCall
    :: ValueRef -> CInt -> IO ()
