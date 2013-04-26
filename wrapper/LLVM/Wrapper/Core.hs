module LLVM.Wrapper.Core
    ( module LLVM.FFI.Core
    , Context
    , getGlobalContext
    , contextCreate

    , MemoryBuffer
    , createMemoryBufferWithContentsOfFile
    , createMemoryBufferWithSTDIN
    , createMemoryBufferWithMemoryRange
    , createMemoryBufferWithMemoryRangeCopy

    -- ** Modules
    , Module
    , moduleCreateWithName
    , moduleCreateWithNameInContext
    , printModuleToFile
    , dumpModule

    -- * Types
    , Type
    , intTypeInContext
    , floatTypeInContext
    , doubleTypeInContext
    , x86FP80TypeInContext
    , voidTypeInContext

    -- ** Function types
    , functionType

    -- ** Struct types
    , structType
    , structTypeInContext
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
    , constStructInContext

    -- ** Scalar constants
    , constRealOfString
    , constString
    , constStringInContext

    -- ** Globals
    , addGlobal
    , getNamedGlobal
    , buildGlobalString
    , buildGlobalStringPtr

    -- ** Pass Manager
    , PassManager
    , createFunctionPassManagerForModule

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
    , appendBasicBlockInContext
    , getBasicBlocks
    , getNextBasicBlock

    -- * Instruction building
    , Builder
    , createBuilder
    , createBuilderInContext
    , getCurrentDebugLocation
    , setCurrentDebugLocation
    , setInstDebugLocation
    , getInsertBlock
    , positionBuilder
    , positionBefore
    , positionAtEnd

    -- ** Control
    , buildRetVoid
    , buildRet
    , buildBr
    , buildIndirectBr
    , buildCondBr
    , buildSwitch
    , buildUnreachable

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
    , constGEP
    , buildStore
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
    , CUInt
    , CULLong
    , CSize
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
import Foreign.ForeignPtr.Safe (ForeignPtr, withForeignPtr, newForeignPtr, newForeignPtr_)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception
import Control.Monad

import LLVM.Wrapper.Internal

import LLVM.FFI.Core
    ( TypeKind(..)
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

    , isConstant, isNull, isUndef

    , constInBoundsGEP
    , constIntCast
    , constIntOfString
    , constIntOfStringAndSize
    , constNSWAdd
    , constPointerCast
    , constPointerNull
    , constRealOfStringAndSize
    , constSExtOrBitCast
    , constZExtOrBitCast
    , constTruncOrBitCast

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

    , initializeFunctionPassManager
    , runFunctionPassManager

    , createModuleProviderForExistingModule

    , getEntryBasicBlock
    , getPreviousBasicBlock
    , getBasicBlockParent

    , setInstrParamAlignment
    , setParamAlignment
    , Attribute(..)

    , getFirstInstruction
    , getNextInstruction
    , getPreviousInstruction
    , getLastInstruction
    , getInstructionParent

    , CallingConvention(..)

    , addCase

    , MetadataKind(..)
    , debugVersion
    )
import qualified LLVM.FFI.Core as FFI

type Type         = FFI.TypeRef
type Value        = FFI.ValueRef
type Builder      = ForeignPtr FFI.Builder
type BasicBlock   = FFI.BasicBlockRef
type Context      = ForeignPtr FFI.Context
type PassManager  = FFI.PassManagerRef
type MemoryBuffer = ForeignPtr FFI.MemoryBuffer

contextCreate :: IO Context
contextCreate = FFI.contextCreate >>= newForeignPtr FFI.ptrContextDispose

getGlobalContext :: IO Context
getGlobalContext = FFI.getGlobalContext >>= newForeignPtr_

createMemoryBufferWithContentsOfFile :: FilePath -> IO MemoryBuffer
createMemoryBufferWithContentsOfFile path =
    alloca $ \bufPtr ->
    alloca $ \msgPtr -> do
      errOccurred <- withCString path $ \cpath ->
                     FFI.createMemoryBufferWithContentsOfFile cpath bufPtr msgPtr
      case errOccurred of
        True -> peek msgPtr >>= peekCString >>= fail
        False -> peek bufPtr >>= newForeignPtr FFI.ptrDisposeMemoryBuffer

createMemoryBufferWithSTDIN :: IO MemoryBuffer
createMemoryBufferWithSTDIN = 
    alloca $ \bufPtr ->
    alloca $ \msgPtr -> do
      errOccurred <- FFI.createMemoryBufferWithSTDIN bufPtr msgPtr
      case errOccurred of
        True -> peek msgPtr >>= peekCString >>= fail
        False -> peek bufPtr >>= newForeignPtr FFI.ptrDisposeMemoryBuffer

createMemoryBufferWithMemoryRange :: Ptr a -> CSize -> String -> Bool -> IO MemoryBuffer
createMemoryBufferWithMemoryRange p len name null =
    (withCString name $ \cname -> FFI.createMemoryBufferWithMemoryRange p len cname null)
    >>= newForeignPtr FFI.ptrDisposeMemoryBuffer

createMemoryBufferWithMemoryRangeCopy :: Ptr a -> CSize -> String -> IO MemoryBuffer
createMemoryBufferWithMemoryRangeCopy p len name =
    (withCString name $ FFI.createMemoryBufferWithMemoryRangeCopy p len)
    >>= newForeignPtr FFI.ptrDisposeMemoryBuffer

moduleCreateWithName :: String -> IO Module
moduleCreateWithName name = initModule =<< withCString name FFI.moduleCreateWithName

moduleCreateWithNameInContext :: String -> Context -> IO Module
moduleCreateWithNameInContext name ctx =
    withForeignPtr ctx $ \ctx' ->
    initModule =<< withCString name (flip FFI.moduleCreateWithNameInContext ctx')

printModuleToFile :: Module -> FilePath -> IO ()
printModuleToFile (MkModule m _) file
    = withCString file
      (\f -> alloca (\msgPtr -> do
                       result <- withForeignPtr m (\modPtr -> FFI.printModuleToFile modPtr f msgPtr)
                       msg <- peek msgPtr
                       case result of
                         False -> return ()
                         True -> do str <- peekCString msg
                                    FFI.disposeMessage msg
                                    fail str))

dumpModule :: Module -> IO ()
dumpModule (MkModule m _) = withForeignPtr m FFI.dumpModule

getTypeByName :: Module -> String -> IO (Maybe Type)
getTypeByName (MkModule m _) name =
    fmap nullableToMaybe $ withForeignPtr m (\mPtr -> withCString name $ FFI.getTypeByName mPtr)

getValueName :: Value -> IO String
getValueName v = FFI.getValueName v >>= peekCString

setValueName :: Value -> String -> IO ()
setValueName v name = withCString name $ FFI.setValueName v

addGlobal :: Module -> Type -> String -> IO Value
addGlobal (MkModule m _) ty name = withForeignPtr m (\mPtr -> withCString name $ FFI.addGlobal mPtr ty)

nullableToMaybe :: Ptr a -> Maybe (Ptr a)
nullableToMaybe p = if p == nullPtr then Nothing else Just p

getNamedGlobal :: Module -> String -> IO (Maybe Value)
getNamedGlobal (MkModule m _) name =
    fmap nullableToMaybe $ withForeignPtr m (\mPtr -> withCString name $ FFI.getNamedGlobal mPtr)

buildGlobalString :: Builder -> String -> String -> IO Value
buildGlobalString b string name
    = withForeignPtr b $ \b' ->
      withCString name (\n -> withCString string (\s -> FFI.buildGlobalString b' s n))

buildGlobalStringPtr :: Builder -> String -> String -> IO Value
buildGlobalStringPtr b string name
    = withForeignPtr b $ \b' ->
      withCString name (\n -> withCString string (\s -> FFI.buildGlobalStringPtr b' s n))

createFunctionPassManagerForModule (MkModule m _) = withForeignPtr m FFI.createFunctionPassManagerForModule

addFunction :: Module -> String -> Type -> IO Value
addFunction (MkModule m _) name ty = withForeignPtr m (\mPtr -> withCString name (\n -> FFI.addFunction mPtr n ty))

getNamedFunction :: Module -> String -> IO (Maybe Value)
getNamedFunction (MkModule m _) name =
    fmap nullableToMaybe $ withForeignPtr m (\mPtr -> withCString name $ FFI.getNamedFunction mPtr)

getParams :: Value -> IO [Value]
getParams f
    = let count = fromIntegral $ FFI.countParams f in
      allocaArray count $ \ptr ->
          FFI.getParams f ptr >> peekArray count ptr

getFunctionCallConv :: Value -> IO CallingConvention
getFunctionCallConv f = fmap FFI.toCallingConvention $ FFI.getFunctionCallConv f

setFunctionCallConv :: Value -> CallingConvention -> IO ()
setFunctionCallConv f c = FFI.setFunctionCallConv f $ FFI.fromCallingConvention c

getInstructionCallConv :: Value -> IO CallingConvention
getInstructionCallConv f = fmap FFI.toCallingConvention $ FFI.getInstructionCallConv f

setInstructionCallConv :: Value -> CallingConvention -> IO ()
setInstructionCallConv f c = FFI.setInstructionCallConv f $ FFI.fromCallingConvention c

intTypeInContext :: Context -> CUInt -> IO Type
intTypeInContext ctx width =
    withForeignPtr ctx $ \ctx' ->
    FFI.intTypeInContext ctx' width
floatTypeInContext :: Context -> IO Type
floatTypeInContext ctx = withForeignPtr ctx FFI.floatTypeInContext
doubleTypeInContext :: Context -> IO Type
doubleTypeInContext ctx = withForeignPtr ctx FFI.doubleTypeInContext
x86FP80TypeInContext :: Context -> IO Type
x86FP80TypeInContext ctx = withForeignPtr ctx FFI.x86FP80TypeInContext
voidTypeInContext :: Context -> IO Type
voidTypeInContext ctx = withForeignPtr ctx FFI.voidTypeInContext

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

structTypeInContext :: Context -> [Type] -> Bool -> IO Type
structTypeInContext ctx types packed =
    withForeignPtr ctx $ \ctx' ->
    withArrayLen types $ \ len ptr ->
        FFI.structTypeInContext ctx' ptr (fromIntegral len) packed

structCreateNamed :: String -> IO Type
structCreateNamed name = do
  ctx <- getGlobalContext
  structCreateNamedInContext ctx name

structCreateNamedInContext :: Context -> String -> IO Type
structCreateNamedInContext ctx name = withForeignPtr ctx $ \ctx' ->
                                      withCString name $ FFI.structCreateNamed ctx'

structSetBody :: Type -> [Type] -> Bool -> IO ()
structSetBody struct body packed
    = withArrayLen body $ \len ptr ->
      FFI.structSetBody struct ptr (fromIntegral len) packed

appendBasicBlock :: Value -> String -> IO BasicBlock
appendBasicBlock function name = withCString name $ FFI.appendBasicBlock function

appendBasicBlockInContext :: Context -> Value -> String -> IO BasicBlock
appendBasicBlockInContext ctx function name =
    withForeignPtr ctx $ \ctx' ->
    withCString name $ FFI.appendBasicBlockInContext ctx' function

getBasicBlocks :: Value -> IO [BasicBlock]
getBasicBlocks v
    = do count <- liftM fromIntegral (FFI.countBasicBlocks v)
         allocaArray count $ \ptr -> do
             FFI.getBasicBlocks v ptr
             peekArray count ptr

getNextBasicBlock :: BasicBlock -> IO (Maybe BasicBlock)
getNextBasicBlock pred = fmap nullableToMaybe $ FFI.getNextBasicBlock pred

getGC :: Value -> IO String
getGC f = FFI.getGC f >>= peekCString

setGC :: Value -> String -> IO ()
setGC f name = withCString name $ FFI.setGC f

getLinkage :: Value -> IO Linkage
getLinkage v = fmap FFI.toLinkage $ FFI.getLinkage v

setLinkage :: Value -> Linkage -> IO ()
setLinkage v l = FFI.setLinkage v (FFI.fromLinkage l)

constStructInContext :: Context -> [Value] -> Bool -> IO Value
constStructInContext ctx values packed =
    withForeignPtr ctx $ \ctx' ->
    withArrayLen values $ \ len ptr ->
        FFI.constStructInContext ctx' ptr (fromIntegral len) packed

-- unsafePerformIO just to wrap the non-effecting withCString call
constRealOfString :: Type -> String -> Value
constRealOfString ty str
    = unsafePerformIO $ withCString str $ \s -> FFI.constRealOfString ty s

-- unsafePerformIO just to wrap the non-effecting withCStringLen call
constString :: String -> Bool -> Value
constString str dontNullTerminate
    = unsafePerformIO $ withCStringLen str $ \(ptr, len) ->
      return $ FFI.constString ptr (fromIntegral len) dontNullTerminate

constStringInContext :: Context -> String -> Bool -> IO Value
constStringInContext ctx str dontNullTerminate =
    withForeignPtr ctx $ \ctx' ->
    withCStringLen str $ \(ptr, len) ->
    FFI.constStringInContext ctx' ptr (fromIntegral len) dontNullTerminate

createBuilder :: IO Builder
createBuilder = FFI.createBuilder >>= newForeignPtr FFI.ptrDisposeBuilder

createBuilderInContext :: Context -> IO Builder
createBuilderInContext ctx =
    withForeignPtr ctx $ \ctx' ->
    FFI.createBuilderInContext ctx' >>= newForeignPtr FFI.ptrDisposeBuilder

getCurrentDebugLocation b = withForeignPtr b FFI.getCurrentDebugLocation
setCurrentDebugLocation b v = withForeignPtr b $ \b' -> FFI.setCurrentDebugLocation b' v
setInstDebugLocation b v = withForeignPtr b $ \b' -> FFI.setInstDebugLocation b' v

getInsertBlock b = withForeignPtr b FFI.getInsertBlock
positionBuilder b bb v = withForeignPtr b $ \b' -> FFI.positionBuilder b' bb v
positionBefore b v = withForeignPtr b $ \b' -> FFI.positionBefore b' v
positionAtEnd b bb = withForeignPtr b $ \b' -> FFI.positionAtEnd b' bb

buildRetVoid b = withForeignPtr b FFI.buildRetVoid
buildRet b v = withForeignPtr b (flip FFI.buildRet v)
buildBr b t = withForeignPtr b (flip FFI.buildBr t)
buildIndirectBr b addr ndests = withForeignPtr b (\b' -> FFI.buildIndirectBr b' addr ndests)
buildCondBr b c t f = withForeignPtr b (\b' -> FFI.buildCondBr b' c t f)
buildSwitch b v d cnt = withForeignPtr b (\b' -> FFI.buildSwitch b' v d cnt)
buildUnreachable b = withForeignPtr b FFI.buildUnreachable
buildStore b v ptr = withForeignPtr b (\b' -> FFI.buildStore b' v ptr)

wrapBin :: (FFI.BuilderRef -> Value -> Value -> CString -> IO Value) ->
            Builder -> Value -> Value -> String  -> IO Value
wrapBin f b x y name = withForeignPtr b (\b' -> withCString name $ f b' x y)

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

wrapUn :: (FFI.BuilderRef -> Value -> CString -> IO Value) ->
           Builder -> Value -> String  -> IO Value
wrapUn f b v name = withForeignPtr b (\b' -> withCString name $ f b' v)

buildNeg    = wrapUn FFI.buildNeg
buildFNeg   = wrapUn FFI.buildFNeg
buildNot    = wrapUn FFI.buildNot
buildNSWNeg = wrapUn FFI.buildNSWNeg
buildNUWNeg = wrapUn FFI.buildNUWNeg

buildICmp :: Builder -> IntPredicate -> Value -> Value -> String -> IO Value
buildICmp b p l r n = withForeignPtr b $ \b' -> withCString n $ FFI.buildICmp b' (FFI.fromIntPredicate p) l r

buildFCmp :: Builder -> FPPredicate -> Value -> Value -> String -> IO Value
buildFCmp b p l r n = withForeignPtr b $ \b' -> withCString n $ FFI.buildFCmp b' (FFI.fromFPPredicate p) l r

buildAlloca :: Builder -> Type -> String -> IO Value
buildAlloca b ty name = withForeignPtr b $ \b' -> withCString name $ FFI.buildAlloca b' ty

buildLoad :: Builder -> Value -> String -> IO Value
buildLoad b ptr name = withForeignPtr b $ \b' -> withCString name $ FFI.buildLoad b' ptr

buildStructGEP :: Builder -> Value -> CUInt -> String -> IO Value
buildStructGEP b s idx name = withForeignPtr b $ \b' -> withCString name $ FFI.buildStructGEP b' s idx

buildInBoundsGEP :: Builder -> Value -> [Value] -> String -> IO Value
buildInBoundsGEP b ptr indices name
    = withArrayLen indices $ \len indicesPtr ->
      withForeignPtr b $ \b' ->
      withCString name $ FFI.buildInBoundsGEP b' ptr indicesPtr $ fromIntegral len

constGEP :: Value -> [Value] -> IO Value
constGEP ptr indices =
    withArrayLen indices $ \len indicesPtr ->
    return $ FFI.constGEP ptr indicesPtr $ fromIntegral len

wrapCast :: (FFI.BuilderRef -> Value -> Type -> CString -> IO Value) ->
             Builder -> Value -> Type -> String  -> IO Value
wrapCast f b v t name = withForeignPtr b (\b' -> withCString name $ f b' v t)

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
buildPhi b ty name = withForeignPtr b (\b' -> withCString name $ FFI.buildPhi b' ty)

addIncoming :: Value -> [(Value, BasicBlock)] -> IO ()
addIncoming phi incoming
    = withArrayLen (map fst incoming) $ \len valPtr ->
      withArray (map snd incoming) $ \blockPtr ->
      FFI.addIncoming phi valPtr blockPtr (fromIntegral len)

buildCall :: Builder -> Value -> [Value] -> String -> IO Value
buildCall b f args name
    = withArrayLen args $ \len ptr ->
      withForeignPtr b (\b' -> withCString name $ FFI.buildCall b' f ptr (fromIntegral len))

buildSelect :: Builder -> Value -> Value -> Value -> String -> IO Value
buildSelect b cond t f name
    = withForeignPtr b (\b' -> withCString name $ FFI.buildSelect b' cond t f)

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
getNamedMetadataOperands (MkModule m _) name =
    withCString name $ \namePtr -> do
      count <- liftM fromIntegral (withForeignPtr m (\m' -> FFI.getNamedMetadataNumOperands m' namePtr))
      allocaArray count $ \ptr -> do
        withForeignPtr m (\m' -> FFI.getNamedMetadataOperands m' namePtr ptr)
        peekArray count ptr

addNamedMetadataOperand :: Module -> String -> Value -> IO ()
addNamedMetadataOperand (MkModule m _) name value =
    withForeignPtr m $ \m' -> withCString name $
                              \n -> FFI.addNamedMetadataOperand m' n value

dumpModuleToString :: Module -> IO String
dumpModuleToString (MkModule m _)= do
  cstr <- withForeignPtr m FFI.dumpModuleToString
  hstr <- peekCString cstr
  FFI.disposeMessage cstr
  return hstr

dumpTypeToString :: Type -> IO String
dumpTypeToString t = do
  cstr <- FFI.dumpTypeToString t
  hstr <- peekCString cstr
  FFI.disposeMessage cstr
  return hstr

dumpValueToString :: Value -> IO String
dumpValueToString v = do
  cstr <- FFI.dumpValueToString v
  hstr <- peekCString cstr
  FFI.disposeMessage cstr
  return hstr
