{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable, CPP #-}

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
    , ptrDisposeModule

    , getDataLayout
    , setDataLayout

    , getTarget
    , setTarget

    -- * Module providers
    , ModuleProvider
    , ModuleProviderRef
    , createModuleProviderForExistingModule
    , ptrDisposeModuleProvider

    -- * Types
    , Type
    , TypeRef
#if HS_LLVM_VERSION < 300
    , addTypeName
    , deleteTypeName
#endif
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
#if HS_LLVM_VERSION < 300
    , opaqueType
#endif

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
#if HS_LLVM_VERSION >= 300
    , structCreateNamed
    , getStructName
    , structSetBody
#endif

    -- * Type handles
#if HS_LLVM_VERSION < 300
    , TypeHandleRef
    , createTypeHandle
    , refineType
    , resolveTypeHandle
    , disposeTypeHandle
#endif

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
    , Linkage(..)
    , fromLinkage
    , toLinkage
    , getLinkage
    , setLinkage

    , Visibility(..)
    , fromVisibility
    , toVisibility
    , getVisibility
    , setVisibility

    , isDeclaration
    , getSection
    , setSection
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
    , constExactSDiv
    , constFAdd
    , constFMul
    , constFNeg
    , constFPCast
    , constFSub
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
    , constNSWMul
    , constNSWNeg
    , constNSWSub
    , constNUWAdd
    , constNUWMul
    , constNUWNeg
    , constNUWSub

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

    -- * Instruction field accessors
    , instGetOpcode, getInstructionOpcode, cmpInstGetPredicate

    -- * Instruction building
    , Builder
    , BuilderRef
    , createBuilder
    , disposeBuilder
    , ptrDisposeBuilder
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
    , buildIndirectBr
    , buildCondBr
    , buildSwitch
    , buildInvoke
#if HS_LLVM_VERSION < 300
    , buildUnwind
#endif
    , buildUnreachable

#if HS_LLVM_VERSION >= 300
    -- ** Landing pad
    , buildLandingPad
    , addClause
    , setCleanup
#endif

    -- ** Arithmetic
    , buildAdd
    , buildSub
    , buildMul
    , buildFAdd
    , buildFMul
    , buildFPCast
    , buildFSub
    , buildUDiv
    , buildSDiv
    , buildExactSDiv
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
    , buildFNeg
    , buildNot
    , buildNSWMul
    , buildNSWNeg
    , buildNSWSub
    , buildNUWAdd
    , buildNUWMul
    , buildNUWNeg
    , buildNUWSub

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
    , buildPointerCast
    , buildTruncOrBitCast
    , buildZExtOrBitCast
    , buildSExtOrBitCast

    , buildPtrDiff

    -- * Misc
    , buildAggregateRet
    , buildGlobalString
    , buildGlobalStringPtr
    , buildInBoundsGEP
    , buildIsNotNull
    , buildIsNull
    , buildNSWAdd
    , buildStructGEP

    -- ** Comparisons
    , IntPredicate(..)
    , toIntPredicate
    , fromIntPredicate
    , FPPredicate(..)
    , toFPPredicate
    , fromFPPredicate
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
    , addAttribute
    , setInstrParamAlignment
    , setParamAlignment
    , Attribute(..)
    , fromAttribute
    , toAttribute
    , addInstrAttribute
    , removeFunctionAttr
    , removeAttribute
    , removeInstrAttribute
    , addFunctionAttr

    -- * Pass manager
    , PassManager
    , PassManagerRef
    , createFunctionPassManager
    , createPassManager
    , disposePassManager
    , ptrDisposePassManager
    , finalizeFunctionPassManager
    , initializeFunctionPassManager
    , runFunctionPassManager
    , runPassManager

    -- * Context functions
    , Context
    , ContextRef

    -- * Debug
    , dumpModule
    , printModuleToFile
    , dumpModuleToString
    , dumpValueToString
    , dumpTypeToString


    -- * Misc
    , alignOf
    , constInBoundsGEP
    , constIntCast
    , constIntOfString
    , constIntOfStringAndSize
    , constNSWAdd
    , constPointerCast
    , constPointerNull
    , constRealOfStringAndSize
    , constSExtOrBitCast

    , getTypeByName
    , insertIntoBuilderWithName

    -- * Context functions
    , moduleCreateWithNameInContext
    , appendBasicBlockInContext
    , insertBasicBlockInContext
    , createBuilderInContext

    , contextDispose

    , constStringInContext
    , constStructInContext
    , constTruncOrBitCast
    , constZExtOrBitCast

    , doubleTypeInContext
    , fP128TypeInContext
    , floatTypeInContext
    , int16TypeInContext
    , int1TypeInContext
    , int32TypeInContext
    , int64TypeInContext
    , int8TypeInContext
    , intTypeInContext
    , labelTypeInContext
#if HS_LLVM_VERSION < 300
    , opaqueTypeInContext
#endif
    , pPCFP128TypeInContext
    , structTypeInContext
    , voidTypeInContext
    , x86FP80TypeInContext
    , getTypeContext

    , addAlias
    , addDestination
    , addGlobalInAddressSpace
    , blockAddress
    , clearInsertionPosition
    , constExtractValue
    , constInlineAsm
    , constInsertValue
    , constIntGetSExtValue
    , constIntGetZExtValue

--    , constUnion
    , contextCreate
--    , countUnionElementTypes
    , createFunctionPassManagerForModule
    , getAttribute
    , getCurrentDebugLocation
    , getFunctionAttr
    , getGlobalContext
    , getMDKindID
    , getMDKindIDInContext
    , getMetadata
    , getOperand
    , getNumOperands
--    , getUnionElementTypes
    , hasMetadata
    , insertIntoBuilder
    , mdNode
    , mdNodeInContext
    , mdString
    , mdStringInContext
    , replaceAllUsesWith
    , setCurrentDebugLocation
    , setInstDebugLocation
    , debugVersion
    , setMetadata
    , MetadataKind(..)
    , fromMetadataKind
    , toMetadataKind
    , getNamedMetadataNumOperands
    , getNamedMetadataOperands
    , addNamedMetadataOperand
--    , unionType
--    , unionTypeInContext

    -- ** Build instruction from opcode
    , buildBinOp
    , getConstOpcode

    , buildCast
    , buildExtractValue
    , buildInsertValue

    -- ** Use
    , OpaqueUse
    , UseRef
    , getFirstUse
    , getNextUse
    , getNumUses
    , getUsedValue
    , getUser
    , isUsedInBasicBlock

    ) where
import Data.Typeable(Typeable)
import Foreign.C.String (CString)
#if __GLASGOW_HASKELL__ >= 704
import Foreign.C.Types (CDouble(..), CInt(..), CUInt(..), CLLong(..), CULLong(..))
#else
import Foreign.C.Types (CDouble, CInt, CUInt, CLLong, CULLong)
#endif
import Foreign.Ptr (Ptr, FunPtr)

#include <llvm-c/Core.h>

data Module
    deriving (Typeable)
type ModuleRef = Ptr Module

foreign import ccall unsafe "LLVMModuleCreateWithName" moduleCreateWithName
    :: CString -> IO ModuleRef

foreign import ccall unsafe "LLVMDisposeModule" disposeModule
    :: ModuleRef -> IO ()

foreign import ccall unsafe "&LLVMDisposeModule" ptrDisposeModule
    :: FunPtr (ModuleRef -> IO ())

foreign import ccall unsafe "LLVMGetDataLayout" getDataLayout
    :: ModuleRef -> IO CString

foreign import ccall unsafe "LLVMSetDataLayout" setDataLayout
    :: ModuleRef -> CString -> IO ()


data ModuleProvider
    deriving (Typeable)
type ModuleProviderRef = Ptr ModuleProvider

foreign import ccall unsafe "LLVMCreateModuleProviderForExistingModule"
    createModuleProviderForExistingModule
    :: ModuleRef -> IO ModuleProviderRef

foreign import ccall unsafe "&LLVMDisposeModuleProvider" ptrDisposeModuleProvider
    :: FunPtr (ModuleProviderRef -> IO ())


data Type
    deriving (Typeable)
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
        -> Bool                 -- ^ non-zero if function is varargs
        -> TypeRef

-- | Indicate whether a function takes varargs.
foreign import ccall unsafe "LLVMIsFunctionVarArg" isFunctionVarArg
        :: TypeRef -> IO Bool

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

#if HS_LLVM_VERSION < 300
foreign import ccall unsafe "LLVMAddTypeName" addTypeName
    :: ModuleRef -> CString -> TypeRef -> IO Bool

foreign import ccall unsafe "LLVMDeleteTypeName" deleteTypeName
    :: ModuleRef -> CString -> IO ()
#endif

-- | Get the type of a sequential type's elements.
foreign import ccall unsafe "LLVMGetElementType" getElementType
    :: TypeRef -> IO TypeRef


data Value
    deriving (Typeable)
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
    :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMSetThreadLocal" setThreadLocal
    :: ValueRef -> Bool -> IO ()

foreign import ccall unsafe "LLVMIsGlobalConstant" isGlobalConstant
    :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMSetGlobalConstant" setGlobalConstant
    :: ValueRef -> Bool -> IO ()

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
    :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMGetUndef" getUndef
    :: TypeRef -> ValueRef

foreign import ccall unsafe "LLVMIsNull" isNull
    :: ValueRef -> IO Bool

foreign import ccall unsafe "LLVMIsUndef" isUndef
    :: ValueRef -> IO Bool

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
                       | GHC
                         deriving (Show, Eq, Ord, Enum, Bounded, Typeable)

fromCallingConvention :: CallingConvention -> CUInt
fromCallingConvention C = (#const LLVMCCallConv)
fromCallingConvention Fast = (#const LLVMFastCallConv)
fromCallingConvention Cold = (#const LLVMColdCallConv)
fromCallingConvention X86StdCall = (#const LLVMX86FastcallCallConv)
fromCallingConvention X86FastCall = (#const LLVMX86StdcallCallConv)
fromCallingConvention GHC = 10

toCallingConvention :: CUInt -> CallingConvention
toCallingConvention c | c == (#const LLVMCCallConv) = C
toCallingConvention c | c == (#const LLVMFastCallConv) = Fast
toCallingConvention c | c == (#const LLVMColdCallConv) = Cold
toCallingConvention c | c == (#const LLVMX86StdcallCallConv) = X86StdCall
toCallingConvention c | c == (#const LLVMX86FastcallCallConv) = X86FastCall
toCallingConvention c | c == 10 = GHC
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
    :: ValueRef -> IO Bool

-- |An enumeration for the kinds of linkage for global values.
data Linkage
    = ExternalLinkage     -- ^Externally visible function
    | AvailableExternallyLinkage 
    | LinkOnceAnyLinkage  -- ^Keep one copy of function when linking (inline)
    | LinkOnceODRLinkage  -- ^Same, but only replaced by something equivalent.
    | WeakAnyLinkage      -- ^Keep one copy of named function when linking (weak)
    | WeakODRLinkage      -- ^Same, but only replaced by something equivalent.
    | AppendingLinkage    -- ^Special purpose, only applies to global arrays
    | InternalLinkage     -- ^Rename collisions when linking (static functions)
    | PrivateLinkage      -- ^Like Internal, but omit from symbol table
    | DLLImportLinkage    -- ^Function to be imported from DLL
    | DLLExportLinkage    -- ^Function to be accessible from DLL
    | ExternalWeakLinkage -- ^ExternalWeak linkage description
    | GhostLinkage        -- ^Stand-in functions for streaming fns from BC files    
    | CommonLinkage       -- ^Tentative definitions
    | LinkerPrivateLinkage -- ^Like Private, but linker removes.
    deriving (Show, Eq, Ord, Enum, Typeable)

fromLinkage :: Linkage -> CUInt
fromLinkage ExternalLinkage             = (#const LLVMExternalLinkage)
fromLinkage AvailableExternallyLinkage  = (#const LLVMAvailableExternallyLinkage )
fromLinkage LinkOnceAnyLinkage          = (#const LLVMLinkOnceAnyLinkage)
fromLinkage LinkOnceODRLinkage          = (#const LLVMLinkOnceODRLinkage)
fromLinkage WeakAnyLinkage              = (#const LLVMWeakAnyLinkage)
fromLinkage WeakODRLinkage              = (#const LLVMWeakODRLinkage)
fromLinkage AppendingLinkage            = (#const LLVMAppendingLinkage)
fromLinkage InternalLinkage             = (#const LLVMInternalLinkage)
fromLinkage PrivateLinkage              = (#const LLVMPrivateLinkage)
fromLinkage DLLImportLinkage            = (#const LLVMDLLImportLinkage)
fromLinkage DLLExportLinkage            = (#const LLVMDLLExportLinkage)
fromLinkage ExternalWeakLinkage         = (#const LLVMExternalWeakLinkage)
fromLinkage GhostLinkage                = (#const LLVMGhostLinkage)
fromLinkage CommonLinkage               = (#const LLVMCommonLinkage)
fromLinkage LinkerPrivateLinkage        = (#const LLVMLinkerPrivateLinkage)

toLinkage :: CUInt -> Linkage
toLinkage c | c == (#const LLVMExternalLinkage)             = ExternalLinkage
toLinkage c | c == (#const LLVMAvailableExternallyLinkage)  = AvailableExternallyLinkage 
toLinkage c | c == (#const LLVMLinkOnceAnyLinkage)          = LinkOnceAnyLinkage
toLinkage c | c == (#const LLVMLinkOnceODRLinkage)          = LinkOnceODRLinkage
toLinkage c | c == (#const LLVMWeakAnyLinkage)              = WeakAnyLinkage
toLinkage c | c == (#const LLVMWeakODRLinkage)              = WeakODRLinkage
toLinkage c | c == (#const LLVMAppendingLinkage)            = AppendingLinkage
toLinkage c | c == (#const LLVMInternalLinkage)             = InternalLinkage
toLinkage c | c == (#const LLVMPrivateLinkage)              = PrivateLinkage
toLinkage c | c == (#const LLVMDLLImportLinkage)            = DLLImportLinkage
toLinkage c | c == (#const LLVMDLLExportLinkage)            = DLLExportLinkage
toLinkage c | c == (#const LLVMExternalWeakLinkage)         = ExternalWeakLinkage
toLinkage c | c == (#const LLVMGhostLinkage)                = GhostLinkage
toLinkage c | c == (#const LLVMCommonLinkage)               = CommonLinkage
toLinkage c | c == (#const LLVMLinkerPrivateLinkage)        = LinkerPrivateLinkage
toLinkage _ = error "toLinkage: bad value"

foreign import ccall unsafe "LLVMGetLinkage" getLinkage
    :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMSetLinkage" setLinkage
    :: ValueRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMGetSection" getSection
    :: ValueRef -> IO CString

foreign import ccall unsafe "LLVMSetSection" setSection
    :: ValueRef -> CString -> IO ()

-- |An enumeration for the kinds of visibility of global values.
data Visibility
    = DefaultVisibility   -- ^The GV is visible
    | HiddenVisibility    -- ^The GV is hidden
    | ProtectedVisibility -- ^The GV is protected
    deriving (Show, Eq, Ord, Enum)

fromVisibility :: Visibility -> CUInt
fromVisibility DefaultVisibility   = (#const LLVMDefaultVisibility)
fromVisibility HiddenVisibility    = (#const LLVMHiddenVisibility)
fromVisibility ProtectedVisibility = (#const LLVMProtectedVisibility)

toVisibility :: CUInt -> Visibility
toVisibility c | c == (#const LLVMDefaultVisibility)   = DefaultVisibility
toVisibility c | c == (#const LLVMHiddenVisibility)    = HiddenVisibility
toVisibility c | c == (#const LLVMProtectedVisibility) = ProtectedVisibility
toVisibility _ = error "toVisibility: bad value"

foreign import ccall unsafe "LLVMGetVisibility" getVisibility
    :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMSetVisibility" setVisibility
    :: ValueRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMGetAlignment" getAlignment
    :: ValueRef -> IO CUInt

foreign import ccall unsafe "LLVMSetAlignment" setAlignment
    :: ValueRef -> CUInt -> IO ()


foreign import ccall unsafe "LLVMConstInt" constInt
    :: TypeRef -> CULLong -> Bool -> ValueRef

foreign import ccall unsafe "LLVMConstReal" constReal
    :: TypeRef -> CDouble -> ValueRef

foreign import ccall unsafe "LLVMConstString" constString
    :: CString -> CUInt -> Bool -> ValueRef

foreign import ccall unsafe "LLVMConstStruct" constStruct
    :: Ptr ValueRef -> CUInt -> Bool -> ValueRef

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

#if HS_LLVM_VERSION < 301
foreign import ccall unsafe "LLVMInstGetOpcode" instGetOpcode
    :: ValueRef -> IO Int
getInstructionOpcode :: ValueRef -> IO Int
getInstructionOpcode = instGetOpcode
#else
foreign import ccall unsafe "LLVMGetInstructionOpcode" getInstructionOpcode
    :: ValueRef -> IO Int
instGetOpcode :: ValueRef -> IO Int
instGetOpcode = getInstructionOpcode
#endif

foreign import ccall unsafe "LLVMCmpInstGetPredicate" cmpInstGetPredicate
    :: ValueRef -> IO Int

data Builder
    deriving (Typeable)
type BuilderRef = Ptr Builder

foreign import ccall unsafe "LLVMCreateBuilder" createBuilder
    :: IO BuilderRef

foreign import ccall unsafe "LLVMDisposeBuilder" disposeBuilder
    :: BuilderRef -> IO ()

foreign import ccall unsafe "&LLVMDisposeBuilder" ptrDisposeBuilder
    :: FunPtr (BuilderRef -> IO ())

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
#if HS_LLVM_VERSION < 300
foreign import ccall unsafe "LLVMBuildUnwind" buildUnwind
    :: BuilderRef -> IO ValueRef
#endif

foreign import ccall unsafe "LLVMBuildUnreachable" buildUnreachable
    :: BuilderRef -> IO ValueRef

#if HS_LLVM_VERSION >= 300
-- New landing pad instructions for LLVM 3.0
foreign import ccall unsafe "LLVMBuildLandingPad" buildLandingPad
    :: BuilderRef -> TypeRef -> ValueRef -> CUInt -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMAddClause" addClause
    :: ValueRef -> ValueRef -> IO ()
foreign import ccall unsafe "LLVMSetCleanup" setCleanup
    :: ValueRef -> CUInt -> IO ()
#endif

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
data IntPredicate =
    IntEQ                       -- ^ equal
  | IntNE                       -- ^ not equal
  | IntUGT                      -- ^ unsigned greater than
  | IntUGE                      -- ^ unsigned greater or equal
  | IntULT                      -- ^ unsigned less than
  | IntULE                      -- ^ unsigned less or equal
  | IntSGT                      -- ^ signed greater than
  | IntSGE                      -- ^ signed greater or equal
  | IntSLT                      -- ^ signed less than
  | IntSLE                      -- ^ signed less or equal
    deriving (Eq, Ord, Enum, Show, Typeable)

fromIntPredicate :: IntPredicate -> CInt
fromIntPredicate p = fromIntegral (fromEnum p + 32)

toIntPredicate :: Int -> IntPredicate
toIntPredicate p = toEnum $ fromIntegral p - 32

data FPPredicate =
    FPFalse           -- ^ Always false (always folded)
  | FPOEQ             -- ^ True if ordered and equal
  | FPOGT             -- ^ True if ordered and greater than
  | FPOGE             -- ^ True if ordered and greater than or equal
  | FPOLT             -- ^ True if ordered and less than
  | FPOLE             -- ^ True if ordered and less than or equal
  | FPONE             -- ^ True if ordered and operands are unequal
  | FPORD             -- ^ True if ordered (no nans)
  | FPUNO             -- ^ True if unordered: isnan(X) | isnan(Y)
  | FPUEQ             -- ^ True if unordered or equal
  | FPUGT             -- ^ True if unordered or greater than
  | FPUGE             -- ^ True if unordered, greater than, or equal
  | FPULT             -- ^ True if unordered or less than
  | FPULE             -- ^ True if unordered, less than, or equal
  | FPUNE             -- ^ True if unordered or not equal
  | FPT               -- ^ Always true (always folded)
    deriving (Eq, Ord, Enum, Show, Typeable)

fromFPPredicate :: FPPredicate -> CInt
fromFPPredicate p = fromIntegral (fromEnum p)

toFPPredicate :: Int -> FPPredicate
toFPPredicate p = toEnum $ fromIntegral p

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
    :: Ptr TypeRef -> CUInt -> Bool -> TypeRef
foreign import ccall unsafe "LLVMCountStructElementTypes"
    countStructElementTypes :: TypeRef -> CUInt
foreign import ccall unsafe "LLVMGetStructElementTypes" getStructElementTypes
    :: TypeRef -> Ptr TypeRef -> IO ()
foreign import ccall unsafe "LLVMIsPackedStruct" isPackedStruct
    :: TypeRef -> Bool

data MemoryBuffer
    deriving (Typeable)
type MemoryBufferRef = Ptr MemoryBuffer

#if HS_LLVM_VERSION < 300
data TypeHandle
    deriving (Typeable)
type TypeHandleRef = Ptr TypeHandle
#endif

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
    deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable)

getTypeKind :: TypeRef -> IO TypeKind
getTypeKind = fmap (toEnum . fromIntegral) . getTypeKindCUInt

foreign import ccall unsafe "LLVMCreateMemoryBufferWithContentsOfFile" createMemoryBufferWithContentsOfFile
    :: CString -> Ptr MemoryBufferRef -> Ptr CString -> IO Bool
foreign import ccall unsafe "LLVMCreateMemoryBufferWithSTDIN" createMemoryBufferWithSTDIN
    :: Ptr MemoryBufferRef -> Ptr CString -> IO Bool
foreign import ccall unsafe "LLVMDisposeMemoryBuffer" disposeMemoryBuffer
    :: MemoryBufferRef -> IO ()
foreign import ccall unsafe "LLVMDisposeMessage" disposeMessage
    :: CString -> IO ()
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
foreign import ccall unsafe "LLVMSetTarget" setTarget
    :: ModuleRef -> CString -> IO ()
foreign import ccall unsafe "LLVMSizeOf" sizeOf
    :: TypeRef -> IO ValueRef

#if HS_LLVM_VERSION < 300
foreign import ccall unsafe "LLVMCreateTypeHandle" createTypeHandle
    :: TypeRef -> IO TypeHandleRef
foreign import ccall unsafe "LLVMDisposeTypeHandle" disposeTypeHandle
    :: TypeHandleRef -> IO ()
foreign import ccall unsafe "LLVMRefineType" refineType
    :: TypeRef -> TypeRef -> IO ()
foreign import ccall unsafe "LLVMResolveTypeHandle" resolveTypeHandle
    :: TypeHandleRef -> IO TypeRef
#else
foreign import ccall unsafe "LLVMStructCreateNamed" structCreateNamed
    :: ContextRef -> CString -> IO TypeRef
foreign import ccall unsafe "LLVMGetStructName" getStructName
    :: TypeRef -> IO CString
foreign import ccall unsafe "LLVMStructSetBody" structSetBody
    :: TypeRef -> Ptr TypeRef -> CUInt -> Bool -> IO ()
#endif

data Attribute
    = ZExtAttribute
    | SExtAttribute
    | NoReturnAttribute
    | InRegAttribute
    | StructRetAttribute
    | NoUnwindAttribute
    | NoAliasAttribute
    | ByValAttribute
    | NestAttribute
    | ReadNoneAttribute
    | ReadOnlyAttribute
    | NoInlineAttribute
    | AlwaysInlineAttribute
    | OptimizeForSizeAttribute
    | StackProtectAttribute
    | StackProtectReqAttribute
    | NoCaptureAttribute
    | NoRedZoneAttribute
    | NoImplicitFloatAttribute
    | NakedAttribute
    deriving (Show, Eq, Ord, Enum, Bounded, Typeable)

fromAttribute :: Attribute -> CAttribute
fromAttribute ZExtAttribute = (#const LLVMZExtAttribute)
fromAttribute SExtAttribute = (#const LLVMSExtAttribute)
fromAttribute NoReturnAttribute = (#const LLVMNoReturnAttribute)
fromAttribute InRegAttribute = (#const LLVMInRegAttribute)
fromAttribute StructRetAttribute = (#const LLVMStructRetAttribute)
fromAttribute NoUnwindAttribute = (#const LLVMNoUnwindAttribute)
fromAttribute NoAliasAttribute = (#const LLVMNoAliasAttribute)
fromAttribute ByValAttribute = (#const LLVMByValAttribute)
fromAttribute NestAttribute = (#const LLVMNestAttribute)
fromAttribute ReadNoneAttribute = (#const LLVMReadNoneAttribute)
fromAttribute ReadOnlyAttribute = (#const LLVMReadOnlyAttribute)
fromAttribute NoInlineAttribute = (#const LLVMNoInlineAttribute)
fromAttribute AlwaysInlineAttribute = (#const LLVMAlwaysInlineAttribute)
fromAttribute OptimizeForSizeAttribute = (#const LLVMOptimizeForSizeAttribute)
fromAttribute StackProtectAttribute = (#const LLVMStackProtectAttribute)
fromAttribute StackProtectReqAttribute = (#const LLVMStackProtectReqAttribute)
fromAttribute NoCaptureAttribute = (#const LLVMNoCaptureAttribute)
fromAttribute NoRedZoneAttribute = (#const LLVMNoRedZoneAttribute)
fromAttribute NoImplicitFloatAttribute = (#const LLVMNoImplicitFloatAttribute)
fromAttribute NakedAttribute = (#const LLVMNakedAttribute)

toAttribute :: CAttribute -> Attribute
toAttribute c | c == (#const LLVMZExtAttribute) = ZExtAttribute
toAttribute c | c == (#const LLVMSExtAttribute) = SExtAttribute
toAttribute c | c == (#const LLVMNoReturnAttribute) = NoReturnAttribute
toAttribute c | c == (#const LLVMInRegAttribute) = InRegAttribute
toAttribute c | c == (#const LLVMStructRetAttribute) = StructRetAttribute
toAttribute c | c == (#const LLVMNoUnwindAttribute) = NoUnwindAttribute
toAttribute c | c == (#const LLVMNoAliasAttribute) = NoAliasAttribute
toAttribute c | c == (#const LLVMByValAttribute) = ByValAttribute
toAttribute c | c == (#const LLVMNestAttribute) = NestAttribute
toAttribute c | c == (#const LLVMReadNoneAttribute) = ReadNoneAttribute
toAttribute c | c == (#const LLVMReadOnlyAttribute) = ReadOnlyAttribute
toAttribute c | c == (#const LLVMNoInlineAttribute) = NoInlineAttribute
toAttribute c | c == (#const LLVMAlwaysInlineAttribute) = AlwaysInlineAttribute
toAttribute c | c == (#const LLVMOptimizeForSizeAttribute) = OptimizeForSizeAttribute
toAttribute c | c == (#const LLVMStackProtectAttribute) = StackProtectAttribute
toAttribute c | c == (#const LLVMStackProtectReqAttribute) = StackProtectReqAttribute
toAttribute c | c == (#const LLVMNoCaptureAttribute) = NoCaptureAttribute
toAttribute c | c == (#const LLVMNoRedZoneAttribute) = NoRedZoneAttribute
toAttribute c | c == (#const LLVMNoImplicitFloatAttribute) = NoImplicitFloatAttribute
toAttribute c | c == (#const LLVMNakedAttribute) = NakedAttribute
toAttribute _ = error "toAttribute: bad value"

type CAttribute = CInt

data PassManager
    deriving (Typeable)
type PassManagerRef = Ptr PassManager

data OpaqueUse
    deriving (Typeable)
type UseRef = Ptr OpaqueUse

foreign import ccall unsafe "LLVMConstRealOfString" constRealOfString
    :: TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMCreateFunctionPassManager" createFunctionPassManager
    :: ModuleProviderRef -> IO PassManagerRef
foreign import ccall unsafe "LLVMCreatePassManager" createPassManager
    :: IO PassManagerRef
foreign import ccall unsafe "LLVMDisposePassManager" disposePassManager
    :: PassManagerRef -> IO ()
foreign import ccall unsafe "&LLVMDisposePassManager" ptrDisposePassManager
    :: FunPtr (PassManagerRef -> IO ())
foreign import ccall unsafe "LLVMDumpModule" dumpModule
    :: ModuleRef -> IO ()
foreign import ccall unsafe "LLVMDumpModuleToString" dumpModuleToString
    :: ModuleRef -> IO CString
foreign import ccall unsafe "LLVMDumpTypeToString" dumpTypeToString
    :: TypeRef -> IO CString
foreign import ccall unsafe "LLVMDumpValueToString" dumpValueToString
    :: ValueRef -> IO CString
foreign import ccall unsafe "LLVMPrintModuleToFile" printModuleToFile
    :: ModuleRef -> CString -> (Ptr CString) -> IO Bool
foreign import ccall unsafe "LLVMFinalizeFunctionPassManager" finalizeFunctionPassManager
    :: PassManagerRef -> IO Bool
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
    :: PassManagerRef -> IO Bool
foreign import ccall unsafe "LLVMLabelType" labelType
    :: TypeRef
foreign import ccall unsafe "LLVMPositionBuilder" positionBuilder
    :: BuilderRef -> BasicBlockRef -> ValueRef -> IO ()
foreign import ccall unsafe "LLVMRunFunctionPassManager" runFunctionPassManager
    :: PassManagerRef -> ValueRef -> IO Bool
foreign import ccall unsafe "LLVMRunPassManager" runPassManager
    :: PassManagerRef -> ModuleRef -> IO Bool
foreign import ccall unsafe "LLVMSetInstrParamAlignment" setInstrParamAlignment
    :: ValueRef -> CUInt -> CUInt -> IO ()
foreign import ccall unsafe "LLVMSetParamAlignment" setParamAlignment
    :: ValueRef -> CUInt -> IO ()

#if HS_LLVM_VERSION < 300
foreign import ccall unsafe "LLVMOpaqueType" opaqueType
    :: TypeRef
foreign import ccall unsafe "LLVMOpaqueTypeInContext" opaqueTypeInContext
    :: ContextRef -> IO TypeRef
#endif


data Context
    deriving (Typeable)
type ContextRef = Ptr Context

foreign import ccall unsafe "LLVMAddAttribute" addAttribute
    :: ValueRef -> CAttribute -> IO ()
foreign import ccall unsafe "LLVMAddInstrAttribute" addInstrAttribute
    :: ValueRef -> CUInt -> CAttribute -> IO ()
foreign import ccall unsafe "LLVMIsTailCall" isTailCall
    :: ValueRef -> IO Bool
foreign import ccall unsafe "LLVMRemoveAttribute" removeAttribute
    :: ValueRef -> CAttribute -> IO ()
foreign import ccall unsafe "LLVMRemoveInstrAttribute" removeInstrAttribute
    :: ValueRef -> CUInt -> CAttribute -> IO ()
foreign import ccall unsafe "LLVMSetTailCall" setTailCall
    :: ValueRef -> Bool -> IO ()
foreign import ccall unsafe "LLVMAddFunctionAttr" addFunctionAttr
    :: ValueRef -> CAttribute -> IO ()
foreign import ccall unsafe "LLVMAlignOf" alignOf
    :: TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMAppendBasicBlockInContext" appendBasicBlockInContext
    :: ContextRef -> ValueRef -> CString -> IO BasicBlockRef
foreign import ccall unsafe "LLVMBuildAggregateRet" buildAggregateRet
    :: BuilderRef -> (Ptr ValueRef) -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMBuildExactSDiv" buildExactSDiv
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFAdd" buildFAdd
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFMul" buildFMul
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFPCast" buildFPCast
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFSub" buildFSub
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFNeg" buildFNeg
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildGlobalString" buildGlobalString
    :: BuilderRef -> CString -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildGlobalStringPtr" buildGlobalStringPtr
    :: BuilderRef -> CString -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildInBoundsGEP" buildInBoundsGEP
    :: BuilderRef -> ValueRef -> (Ptr ValueRef) -> CUInt -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildIsNotNull" buildIsNotNull
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildIsNull" buildIsNull
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNSWAdd" buildNSWAdd
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildPointerCast" buildPointerCast
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildPtrDiff" buildPtrDiff
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildSExtOrBitCast" buildSExtOrBitCast
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildStructGEP" buildStructGEP
    :: BuilderRef -> ValueRef -> CUInt -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildTruncOrBitCast" buildTruncOrBitCast
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildZExtOrBitCast" buildZExtOrBitCast
    :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMConstExactSDiv" constExactSDiv
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstFAdd" constFAdd
    :: ValueRef -> ValueRef -> ValueRef
foreign import ccall unsafe "LLVMConstFMul" constFMul
    :: ValueRef -> ValueRef -> ValueRef
foreign import ccall unsafe "LLVMConstFNeg" constFNeg
    :: ValueRef -> ValueRef
foreign import ccall unsafe "LLVMConstFPCast" constFPCast
    :: ValueRef -> TypeRef -> ValueRef
foreign import ccall unsafe "LLVMConstFSub" constFSub
    :: ValueRef -> ValueRef -> ValueRef
foreign import ccall unsafe "LLVMConstInBoundsGEP" constInBoundsGEP
    :: ValueRef -> (Ptr ValueRef) -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMConstIntCast" constIntCast
    :: ValueRef -> TypeRef -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMConstIntOfString" constIntOfString
    :: TypeRef -> CString -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMConstIntOfStringAndSize" constIntOfStringAndSize
    :: TypeRef -> CString -> CUInt -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMConstNSWAdd" constNSWAdd
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstPointerCast" constPointerCast
    :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstPointerNull" constPointerNull
    :: TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstRealOfStringAndSize" constRealOfStringAndSize
    :: TypeRef -> CString -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMConstSExtOrBitCast" constSExtOrBitCast
    :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstStringInContext" constStringInContext
    :: ContextRef -> CString -> CUInt -> Bool -> IO ValueRef
foreign import ccall unsafe "LLVMConstStructInContext" constStructInContext
    :: ContextRef -> (Ptr ValueRef) -> CUInt -> Bool -> IO ValueRef
foreign import ccall unsafe "LLVMConstTruncOrBitCast" constTruncOrBitCast
    :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstZExtOrBitCast" constZExtOrBitCast
    :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMContextDispose" contextDispose
    :: ContextRef -> IO ()
foreign import ccall unsafe "LLVMCreateBuilderInContext" createBuilderInContext
    :: ContextRef -> IO BuilderRef
foreign import ccall unsafe "LLVMDoubleTypeInContext" doubleTypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMFP128TypeInContext" fP128TypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMFloatTypeInContext" floatTypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMGetTypeByName" getTypeByName
    :: ModuleRef -> CString -> IO TypeRef
foreign import ccall unsafe "LLVMGetNamedMetadataNumOperands" getNamedMetadataNumOperands
    :: ModuleRef -> CString -> IO CUInt
foreign import ccall unsafe "LLVMGetNamedMetadataOperands" getNamedMetadataOperands
    :: ModuleRef -> CString -> (Ptr ValueRef) -> IO ()
foreign import ccall unsafe "LLVMAddNamedMetadataOperand" addNamedMetadataOperand
    :: ModuleRef -> CString -> ValueRef -> IO ()
foreign import ccall unsafe "LLVMGetTypeContext" getTypeContext
    :: TypeRef -> IO ContextRef
foreign import ccall unsafe "LLVMInsertBasicBlockInContext" insertBasicBlockInContext
    :: ContextRef -> BasicBlockRef -> CString -> IO BasicBlockRef
foreign import ccall unsafe "LLVMInsertIntoBuilderWithName" insertIntoBuilderWithName
    :: BuilderRef -> ValueRef -> CString -> IO ()
foreign import ccall unsafe "LLVMInt16TypeInContext" int16TypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMInt1TypeInContext" int1TypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMInt32TypeInContext" int32TypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMInt64TypeInContext" int64TypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMInt8TypeInContext" int8TypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMIntTypeInContext" intTypeInContext
    :: ContextRef -> CUInt -> IO TypeRef
foreign import ccall unsafe "LLVMLabelTypeInContext" labelTypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMModuleCreateWithNameInContext" moduleCreateWithNameInContext
    :: CString -> ContextRef -> IO ModuleRef
foreign import ccall unsafe "LLVMPPCFP128TypeInContext" pPCFP128TypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMRemoveFunctionAttr" removeFunctionAttr
    :: ValueRef -> CAttribute -> IO ()
foreign import ccall unsafe "LLVMStructTypeInContext" structTypeInContext
    :: ContextRef -> (Ptr TypeRef) -> CUInt -> Bool -> IO TypeRef
foreign import ccall unsafe "LLVMVoidTypeInContext" voidTypeInContext
    :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMX86FP80TypeInContext" x86FP80TypeInContext
    :: ContextRef -> IO TypeRef




foreign import ccall unsafe "LLVMAddAlias" addAlias
    :: ModuleRef -> TypeRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMAddDestination" addDestination
    :: ValueRef -> BasicBlockRef -> IO ()
foreign import ccall unsafe "LLVMAddGlobalInAddressSpace" addGlobalInAddressSpace
    :: ModuleRef -> TypeRef -> CString -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMBlockAddress" blockAddress
    :: ValueRef -> BasicBlockRef -> IO ValueRef
foreign import ccall unsafe "LLVMBuildBinOp" buildBinOp
    :: BuilderRef -> CUInt{-Opcode-} -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildCast" buildCast
    :: BuilderRef -> CUInt{-Opcode-} -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildExtractValue" buildExtractValue
    :: BuilderRef -> ValueRef -> CUInt -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildIndirectBr" buildIndirectBr
    :: BuilderRef -> ValueRef -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMBuildInsertValue" buildInsertValue
    :: BuilderRef -> ValueRef -> ValueRef -> CUInt -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNSWMul" buildNSWMul
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNSWNeg" buildNSWNeg
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNSWSub" buildNSWSub
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNUWAdd" buildNUWAdd
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNUWMul" buildNUWMul
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNUWNeg" buildNUWNeg
    :: BuilderRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNUWSub" buildNUWSub
    :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMClearInsertionPosition" clearInsertionPosition
    :: BuilderRef -> IO ()
foreign import ccall unsafe "LLVMConstExtractValue" constExtractValue
    :: ValueRef -> Ptr CUInt -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMConstInlineAsm" constInlineAsm
    :: TypeRef -> CString -> CString -> Bool -> Bool -> IO ValueRef
foreign import ccall unsafe "LLVMConstInsertValue" constInsertValue
    :: ValueRef -> ValueRef -> Ptr CUInt -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMConstIntGetSExtValue" constIntGetSExtValue
    :: ValueRef -> IO CLLong
foreign import ccall unsafe "LLVMConstIntGetZExtValue" constIntGetZExtValue
    :: ValueRef -> IO CULLong
foreign import ccall unsafe "LLVMConstNSWMul" constNSWMul
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstNSWNeg" constNSWNeg
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstNSWSub" constNSWSub
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstNUWAdd" constNUWAdd
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstNUWMul" constNUWMul
    :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstNUWNeg" constNUWNeg
    :: ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstNUWSub" constNUWSub
    :: ValueRef -> ValueRef -> IO ValueRef
{-
foreign import ccall unsafe "LLVMConstUnion" constUnion
    :: TypeRef -> ValueRef -> IO ValueRef
-}
foreign import ccall unsafe "LLVMContextCreate" contextCreate
    :: IO ContextRef
{-
foreign import ccall unsafe "LLVMCountUnionElementTypes" countUnionElementTypes
    :: TypeRef -> IO CUInt
-}
foreign import ccall unsafe "LLVMCreateFunctionPassManagerForModule" createFunctionPassManagerForModule
    :: ModuleRef -> IO PassManagerRef
foreign import ccall unsafe "LLVMGetAttribute" getAttribute
    :: ValueRef -> IO CUInt{-Attribute-}
foreign import ccall unsafe "LLVMGetConstOpcode" getConstOpcode
    :: ValueRef -> IO CUInt {-Opcode-}
foreign import ccall unsafe "LLVMGetCurrentDebugLocation" getCurrentDebugLocation
    :: BuilderRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetFirstUse" getFirstUse
    :: ValueRef -> IO UseRef
foreign import ccall unsafe "LLVMGetFunctionAttr" getFunctionAttr
    :: ValueRef -> IO CUInt {-Attribute-}
foreign import ccall unsafe "LLVMGetGlobalContext" getGlobalContext
    :: IO ContextRef
foreign import ccall unsafe "LLVMGetMDKindID" getMDKindID
    :: CString -> CUInt -> IO CUInt
foreign import ccall unsafe "LLVMGetMDKindIDInContext" getMDKindIDInContext
    :: ContextRef -> CString -> CUInt -> IO CUInt
foreign import ccall unsafe "LLVMGetMetadata" getMetadata
    :: ValueRef -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMGetNextUse" getNextUse
    :: UseRef -> IO UseRef
foreign import ccall unsafe "LLVMGetOperand" getOperand
    :: ValueRef -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMGetNumOperands" getNumOperands
    :: ValueRef -> IO CUInt
{-
foreign import ccall unsafe "LLVMGetUnionElementTypes" getUnionElementTypes
    :: TypeRef -> (Ptr TypeRef) -> IO ()
-}
foreign import ccall unsafe "LLVMValueIsUsedInBasicBlock" isUsedInBasicBlock
    :: BasicBlockRef -> ValueRef -> IO Bool
foreign import ccall unsafe "LLVMValueGetNumUses" getNumUses
    :: ValueRef -> IO CUInt
foreign import ccall unsafe "LLVMGetUsedValue" getUsedValue
    :: UseRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetUser" getUser
    :: UseRef -> IO ValueRef
foreign import ccall unsafe "LLVMHasMetadata" hasMetadata
    :: ValueRef -> IO Bool
foreign import ccall unsafe "LLVMInsertIntoBuilder" insertIntoBuilder
    :: BuilderRef -> ValueRef -> IO ()
foreign import ccall unsafe "LLVMMDNode" mdNode
    :: (Ptr ValueRef) -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMMDNodeInContext" mdNodeInContext
    :: ContextRef -> (Ptr ValueRef) -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMMDString" mdString
    :: CString -> CUInt -> ValueRef
foreign import ccall unsafe "LLVMMDStringInContext" mdStringInContext
    :: ContextRef -> CString -> CUInt -> ValueRef
foreign import ccall unsafe "LLVMReplaceAllUsesWith" replaceAllUsesWith
    :: ValueRef -> ValueRef -> IO ()
foreign import ccall unsafe "LLVMSetCurrentDebugLocation" setCurrentDebugLocation
    :: BuilderRef -> ValueRef -> IO ()
foreign import ccall unsafe "LLVMSetInstDebugLocation" setInstDebugLocation
    :: BuilderRef -> ValueRef -> IO ()
foreign import ccall unsafe "LLVMSetMetadata" setMetadata
    :: ValueRef -> CUInt -> ValueRef -> IO ()

data MetadataKind = Dbg | TBAA | Prof | FPMath | Range | TBAAStruct
    deriving (Show, Eq, Enum)

toMetadataKind :: CUInt -> MetadataKind
toMetadataKind c = toEnum $ fromIntegral c

fromMetadataKind :: MetadataKind -> CUInt
fromMetadataKind k = fromIntegral $ fromEnum k

debugVersion :: (Integral a) => a
debugVersion = 786432 -- 12 << 16

{-
foreign import ccall unsafe "LLVMUnionType" unionType
    :: (Ptr TypeRef) -> CUInt -> IO TypeRef
foreign import ccall unsafe "LLVMUnionTypeInContext" unionTypeInContext
    :: ContextRef -> (Ptr TypeRef) -> CUInt -> IO TypeRef
-}
