module LLVM.Wrapper.Core
    ( module LLVM.FFI.Core
    -- ** Modules
    , Module
    , moduleCreateWithName
    , withModule
    , printModuleToFile

    -- * Types
    , Type

    -- ** Function types
    , functionType

    -- ** Struct types
    , structType
    , structCreateNamed
    , structCreateNamedInContext
    , structSetBody

    -- ** Misc
    , getTypeByName

    -- * Values
    , Value
    , getValueName
    , setValueName
    , getGC
    , setGC
    , getLinkage
    , setLinkage

    -- ** Scalar constants
    , constRealOfString
    , constString

    -- ** Globals
    , addGlobal
    , getNamedGlobal
    , buildGlobalString
    , buildGlobalStringPtr

    -- ** Pass Manager
    , PassManager

    -- ** Functions
    , addFunction
    , getNamedFunction
    , getParams
    , getFunctionCallConv
    , setFunctionCallConv
    , getInstructionCallConv
    , setInstructionCallConv
    , addAttribute
    , removeAttribute
    , addFunctionAttr

    -- ** Metadata
    , setMetadata
    , getMetadata
    , getNamedMetadataOperands
    , addNamedMetadataOperand
    , mdNode
    , mdString

    -- * Basic blocks
    , BasicBlock
    , appendBasicBlock
    , getBasicBlocks

    -- * Instruction building
    , Builder
    , withBuilder

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
    , buildNSWAdd
    , buildNSWMul
    , buildNSWNeg
    , buildNSWSub
    , buildNUWAdd
    , buildNUWMul
    , buildNUWNeg
    , buildNUWSub
    , buildFCmp
    , buildICmp

    -- ** Memory
    , buildAlloca
    , buildLoad
    , buildStructGEP
    , buildInBoundsGEP
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

    -- ** Misc
    , buildPhi
    , addIncoming
    , buildCall
    , buildSelect
    , isUnreachable

    -- * Debug
    , dumpModuleToString
    , dumpValueToString
    , dumpTypeToString
    ) where

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Foreign.ForeignPtr.Safe
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception
import Control.Monad

import LLVM.FFI.Core
    ( disposeModule
    , dumpModule

    , TypeKind(..)
    , getTypeKind

    , int1Type
    , int8Type
    , int16Type
    , int32Type
    , int64Type
    , integerType
    , getIntTypeWidth

    , floatType
    , doubleType
    , x86FP80Type
    , fp128Type
    , ppcFP128Type

    , arrayType
    , pointerType
    , vectorType
    , voidType

    , typeOf
    , dumpValue
    , constNull
    , constPointerNull
    , getUndef
    , constInt
    , constReal

    , Linkage(..)

    , IntPredicate(..)
    , FPPredicate(..)

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
    , constNSWMul
    , constNSWNeg
    , constNSWSub
    , constNUWAdd
    , constNUWMul
    , constNUWNeg
    , constNUWSub

    , isTailCall
    , setTailCall
    , deleteFunction

    , createFunctionPassManagerForModule
    , initializeFunctionPassManager
    , runFunctionPassManager

    , createModuleProviderForExistingModule

    , getEntryBasicBlock
    , getNextBasicBlock
    , getPreviousBasicBlock
    , getBasicBlockParent

    , setInstrParamAlignment
    , setParamAlignment
    , Attribute(..)

    , createBuilder
    , disposeBuilder
    , getInsertBlock
    , positionBuilder
    , positionBefore
    , positionAtEnd
    , getFirstInstruction
    , getNextInstruction
    , getPreviousInstruction
    , getLastInstruction
    , getInstructionParent

    , CallingConvention(..)

    , buildRetVoid
    , buildRet
    , buildBr
    , buildIndirectBr
    , buildCondBr
    , buildSwitch
    , buildUnreachable
    , addCase

    , buildStore

    , MetadataKind(..)
    , getCurrentDebugLocation
    , setCurrentDebugLocation
    , setInstDebugLocation
    , debugVersion
    )
import qualified LLVM.FFI.Core as FFI

type Type         = FFI.TypeRef
type Module       = FFI.ModuleRef
type Value        = FFI.ValueRef
type Builder      = FFI.BuilderRef
type BasicBlock   = FFI.BasicBlockRef
type Context      = FFI.ContextRef
type PassManager  = FFI.PassManagerRef

moduleCreateWithName :: String -> IO Module
moduleCreateWithName name = withCString name FFI.moduleCreateWithName

withModule :: String -> (Module -> IO a) -> IO a
withModule n f = do m <- moduleCreateWithName n
                    finally (f m) (disposeModule m)

printModuleToFile :: Module -> FilePath -> IO ()
printModuleToFile m file
    = withCString file
      (\f -> alloca (\msgPtr -> do
                       result <- FFI.printModuleToFile m f msgPtr
                       msg <- peek msgPtr
                       case result of
                         False -> return ()
                         True -> do str <- peekCString msg
                                    FFI.disposeMessage msg
                                    fail str))

getTypeByName :: Module -> String -> IO (Maybe Type)
getTypeByName m name = fmap nullableToMaybe $ withCString name $ FFI.getTypeByName m

getValueName :: Value -> IO String
getValueName v = FFI.getValueName v >>= peekCString

setValueName :: Value -> String -> IO ()
setValueName v name = withCString name $ FFI.setValueName v

addGlobal :: Module -> Type -> String -> IO Value
addGlobal m ty name = withCString name $ FFI.addGlobal m ty

nullableToMaybe :: Ptr a -> Maybe (Ptr a)
nullableToMaybe p = if p == nullPtr then Nothing else Just p

getNamedGlobal :: Module -> String -> IO (Maybe Value)
getNamedGlobal m name = fmap nullableToMaybe $ withCString name $ FFI.getNamedGlobal m

buildGlobalString :: Builder -> String -> String -> IO Value
buildGlobalString b string name
    = withCString name (\n -> withCString string (\s -> FFI.buildGlobalString b s n))

buildGlobalStringPtr :: Builder -> String -> String -> IO Value
buildGlobalStringPtr b string name
    = withCString name (\n -> withCString string (\s -> FFI.buildGlobalStringPtr b s n))

addFunction :: Module -> String -> Type -> IO Value
addFunction m name ty = withCString name (\n -> FFI.addFunction m n ty)

getNamedFunction :: Module -> String -> IO (Maybe Value)
getNamedFunction m name = fmap nullableToMaybe $ withCString name $ FFI.getNamedFunction m

getParams :: Value -> IO [Value]
getParams f
    = do let count = fromIntegral $ FFI.countParams f
         allocaArray count $ \ptr -> do
           FFI.getParams f ptr
           peekArray count ptr

getFunctionCallConv :: Value -> IO CallingConvention
getFunctionCallConv f = fmap FFI.toCallingConvention $ FFI.getFunctionCallConv f

setFunctionCallConv :: Value -> CallingConvention -> IO ()
setFunctionCallConv f c = FFI.setFunctionCallConv f $ FFI.fromCallingConvention c

getInstructionCallConv :: Value -> IO CallingConvention
getInstructionCallConv f = fmap FFI.toCallingConvention $ FFI.getInstructionCallConv f

setInstructionCallConv :: Value -> CallingConvention -> IO ()
setInstructionCallConv f c = FFI.setInstructionCallConv f $ FFI.fromCallingConvention c

-- unsafePerformIO just to wrap the non-effecting withArrayLen call
functionType :: Type -> [Type] -> Bool -> Type
functionType returnTy argTys isVarArg
    = unsafePerformIO $ withArrayLen argTys $ \len ptr ->
      return $ FFI.functionType returnTy ptr (fromIntegral len) isVarArg

-- unsafePerformIO just to wrap the non-effecting withArrayLen call
structType :: [Type] -> Bool -> Type
structType types packed = unsafePerformIO $
    withArrayLen types $ \ len ptr ->
        return $ FFI.structType ptr (fromIntegral len) packed

structCreateNamed :: String -> IO Type
structCreateNamed name
    = do ctx <- FFI.getGlobalContext
         structCreateNamedInContext ctx name

structCreateNamedInContext :: Context -> String -> IO Type
structCreateNamedInContext ctx name = withCString name $ FFI.structCreateNamed ctx

structSetBody :: Type -> [Type] -> Bool -> IO ()
structSetBody struct body packed
    = withArrayLen body $ \len ptr ->
      FFI.structSetBody struct ptr (fromIntegral len) packed

appendBasicBlock :: Value -> String -> IO BasicBlock
appendBasicBlock function name = withCString name $ FFI.appendBasicBlock function

getBasicBlocks :: Value -> IO [BasicBlock]
getBasicBlocks v
    = do count <- liftM fromIntegral (FFI.countBasicBlocks v)
         allocaArray count $ \ptr -> do
             FFI.getBasicBlocks v ptr
             peekArray count ptr

getGC :: Value -> IO String
getGC f = FFI.getGC f >>= peekCString

setGC :: Value -> String -> IO ()
setGC f name = withCString name $ FFI.setGC f

getLinkage :: Value -> IO Linkage
getLinkage v = fmap FFI.toLinkage $ FFI.getLinkage v

setLinkage :: Value -> Linkage -> IO ()
setLinkage v l = FFI.setLinkage v (FFI.fromLinkage l)

-- unsafePerformIO just to wrap the non-effecting withCString call
constRealOfString :: Type -> String -> Value
constRealOfString ty str
    = unsafePerformIO $ withCString str $ \s -> FFI.constRealOfString ty s

-- unsafePerformIO just to wrap the non-effecting withCStringLen call
constString :: String -> Bool -> Value
constString str dontNullTerminate
    = unsafePerformIO $ withCStringLen str $ \(ptr, len) ->
      return $ FFI.constString ptr (fromIntegral len) dontNullTerminate

withBuilder :: (Builder -> IO a) -> IO a
withBuilder f = do p <- createBuilder
                   finally (f p) (disposeBuilder p)

wrapBin :: (Builder -> Value -> Value -> CString -> IO Value) ->
            Builder -> Value -> Value -> String  -> IO Value
wrapBin f b x y name = withCString name $ f b x y

buildAdd       = wrapBin FFI.buildAdd
buildSub       = wrapBin FFI.buildSub
buildMul       = wrapBin FFI.buildMul
buildNSWAdd    = wrapBin FFI.buildNSWAdd
buildNSWSub    = wrapBin FFI.buildNSWSub
buildNSWMul    = wrapBin FFI.buildNSWMul
buildNUWAdd    = wrapBin FFI.buildNUWAdd
buildNUWSub    = wrapBin FFI.buildNUWSub
buildNUWMul    = wrapBin FFI.buildNUWMul
buildUDiv      = wrapBin FFI.buildUDiv
buildSDiv      = wrapBin FFI.buildSDiv
buildExactSDiv = wrapBin FFI.buildExactSDiv
buildURem      = wrapBin FFI.buildURem
buildSRem      = wrapBin FFI.buildSRem
buildFAdd      = wrapBin FFI.buildFAdd
buildFSub      = wrapBin FFI.buildFSub
buildFMul      = wrapBin FFI.buildFMul
buildFDiv      = wrapBin FFI.buildFDiv
buildFRem      = wrapBin FFI.buildFRem
buildShl       = wrapBin FFI.buildShl
buildLShr      = wrapBin FFI.buildLShr
buildAShr      = wrapBin FFI.buildAShr
buildAnd       = wrapBin FFI.buildAnd
buildOr        = wrapBin FFI.buildOr
buildXor       = wrapBin FFI.buildXor

wrapUn :: (Builder -> Value -> CString -> IO Value) ->
           Builder -> Value -> String  -> IO Value
wrapUn f b v name = withCString name $ f b v

buildNeg    = wrapUn FFI.buildNeg
buildFNeg   = wrapUn FFI.buildFNeg
buildNot    = wrapUn FFI.buildNot
buildNSWNeg = wrapUn FFI.buildNSWNeg
buildNUWNeg = wrapUn FFI.buildNUWNeg

buildICmp :: Builder -> IntPredicate -> Value -> Value -> String -> IO Value
buildICmp b p l r n = withCString n $ FFI.buildICmp b (FFI.fromIntPredicate p) l r

buildFCmp :: Builder -> FPPredicate -> Value -> Value -> String -> IO Value
buildFCmp b p l r n = withCString n $ FFI.buildFCmp b (FFI.fromFPPredicate p) l r

buildAlloca :: Builder -> Type -> String -> IO Value
buildAlloca b ty name = withCString name $ FFI.buildAlloca b ty

buildLoad :: Builder -> Value -> String -> IO Value
buildLoad b ptr name = withCString name $ FFI.buildLoad b ptr

buildStructGEP :: Builder -> Value -> CUInt -> String -> IO Value
buildStructGEP b s idx name = withCString name $ FFI.buildStructGEP b s idx

buildInBoundsGEP :: Builder -> Value -> [Value] -> String -> IO Value
buildInBoundsGEP b ptr indices name
    = withArrayLen indices $ \len indicesPtr ->
      withCString name $ FFI.buildInBoundsGEP b ptr indicesPtr $ fromIntegral len

wrapCast :: (Builder -> Value -> Type -> CString -> IO Value) ->
             Builder -> Value -> Type -> String  -> IO Value
wrapCast f b v t name = withCString name $ f b v t

buildTrunc          = wrapCast FFI.buildTrunc
buildZExt           = wrapCast FFI.buildZExt
buildSExt           = wrapCast FFI.buildSExt
buildFPToUI         = wrapCast FFI.buildFPToUI
buildFPToSI         = wrapCast FFI.buildFPToSI
buildUIToFP         = wrapCast FFI.buildUIToFP
buildSIToFP         = wrapCast FFI.buildSIToFP
buildFPTrunc        = wrapCast FFI.buildFPTrunc
buildFPExt          = wrapCast FFI.buildFPExt
buildPtrToInt       = wrapCast FFI.buildPtrToInt
buildIntToPtr       = wrapCast FFI.buildIntToPtr
buildBitCast        = wrapCast FFI.buildBitCast
buildPointerCast    = wrapCast FFI.buildPointerCast
buildTruncOrBitCast = wrapCast FFI.buildTruncOrBitCast
buildZExtOrBitCast  = wrapCast FFI.buildZExtOrBitCast
buildSExtOrBitCast  = wrapCast FFI.buildSExtOrBitCast
buildFPCast         = wrapCast FFI.buildFPCast

buildPhi :: Builder -> Type -> String -> IO Value
buildPhi b ty name = withCString name $ FFI.buildPhi b ty

addIncoming :: Value -> [(Value, BasicBlock)] -> IO ()
addIncoming phi incoming
    = withArrayLen (map fst incoming) $ \len valPtr ->
      withArray (map snd incoming) $ \blockPtr ->
      FFI.addIncoming phi valPtr blockPtr (fromIntegral len)

buildCall :: Builder -> Value -> [Value] -> String -> IO Value
buildCall b f args name
    = withArrayLen args $ \len ptr ->
      withCString name $ FFI.buildCall b f ptr (fromIntegral len)

buildSelect :: Builder -> Value -> Value -> Value -> String -> IO Value
buildSelect b cond t f name
    = withCString name $ FFI.buildSelect b cond t f

-- See LLVMOpcode in llvm-c/Core.h
isUnreachable :: Value -> IO Bool
isUnreachable v = fmap (== 7) $ FFI.getInstructionOpcode v

addAttribute :: Value -> Attribute -> IO ()
addAttribute v a = FFI.addAttribute v $ FFI.fromAttribute a

removeAttribute :: Value -> Attribute -> IO ()
removeAttribute v a = FFI.removeAttribute v $ FFI.fromAttribute a

addFunctionAttr :: Value -> Attribute -> IO ()
addFunctionAttr v a = FFI.addFunctionAttr v $ FFI.fromAttribute a

setMetadata :: Value -> MetadataKind -> Value -> IO ()
setMetadata instruction kind node = FFI.setMetadata instruction (FFI.fromMetadataKind kind) node

getMetadata :: Value -> MetadataKind -> IO Value
getMetadata instruction kind = FFI.getMetadata instruction (FFI.fromMetadataKind kind)

mdNode :: [Value] -> IO Value
mdNode children = withArrayLen children $ \len ptr -> FFI.mdNode ptr $ fromIntegral len

-- unsafePerformIO just to wrap the non-effecting withCString call
mdString :: String -> Value
mdString s = unsafePerformIO $
             withCStringLen s $ \(ptr, len) -> return $ FFI.mdString ptr $ fromIntegral len

getNamedMetadataOperands :: Module -> String -> IO [Value]
getNamedMetadataOperands m name
    = withCString name $ \namePtr -> do
        count <- liftM fromIntegral (FFI.getNamedMetadataNumOperands m namePtr)
        allocaArray count $ \ptr -> do
          FFI.getNamedMetadataOperands m namePtr ptr
          peekArray count ptr

addNamedMetadataOperand :: Module -> String -> Value -> IO ()
addNamedMetadataOperand m name value = withCString name $ \n -> FFI.addNamedMetadataOperand m n value

dumpModuleToString :: Module -> IO String
dumpModuleToString m = do cstr <- FFI.dumpModuleToString m
                          hstr <- peekCString cstr
                          FFI.disposeMessage cstr
                          return hstr

dumpTypeToString :: Type -> IO String
dumpTypeToString m = do cstr <- FFI.dumpTypeToString m
                        hstr <- peekCString cstr
                        FFI.disposeMessage cstr
                        return hstr

dumpValueToString :: Value -> IO String
dumpValueToString m = do cstr <- FFI.dumpValueToString m
                         hstr <- peekCString cstr
                         FFI.disposeMessage cstr
                         return hstr
