module LLVM.Wrapper.Core
    ( module LLVM.FFI.Core
    -- ** Modules
    , Module
    , moduleCreateWithName
    , withModule

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
    , getLinkage
    , setLinkage
    
    -- ** Scalar constants
    , constInt
    , constString

    -- ** Globals
    , addGlobal
    , getNamedGlobal
    , buildGlobalString
    , buildGlobalStringPtr
    -- ** Functions
    , addFunction
    , getNamedFunction
    , getParams
    , isTailCall
    , setTailCall
    , getFunctionCallConv
    , setFunctionCallConv
    , getInstructionCallConv
    , setInstructionCallConv

    -- * Basic blocks
    , BasicBlock
    , appendBasicBlock

    -- * Instruction building
    , Builder
    , withBuilder

    -- ** Arithmetic
    , buildAdd
    , buildSub
    , buildMul
    , buildUDiv
    , buildSDiv
    , buildFAdd
    , buildFSub
    , buildFMul
    , buildFDiv
    , IntPredicate(..)
    , buildICmp
    , FPPredicate(..)
    , buildFCmp

    -- ** Memory
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
    ) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
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
    , constReal

    , Linkage(..)

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

    , createBuilder
    , disposeBuilder
    , positionAtEnd
    , getInsertBlock

    , CallingConvention(..)

    , buildRetVoid
    , buildRet
    , buildBr
    , buildIndirectBr
    , buildCondBr
    , buildSwitch
    , addCase

    , buildStore
    )
import qualified LLVM.FFI.Core as FFI

type Type       = FFI.TypeRef
type Module     = FFI.ModuleRef
type Value      = FFI.ValueRef
type Builder    = FFI.BuilderRef
type BasicBlock = FFI.BasicBlockRef
type Context    = FFI.ContextRef

moduleCreateWithName :: String -> IO Module
moduleCreateWithName name = withCString name FFI.moduleCreateWithName

withModule :: String -> (Module -> IO a) -> IO a
withModule n f = do m <- moduleCreateWithName n
                    finally (f m) (disposeModule m)

getTypeByName :: Module -> String -> IO Type
getTypeByName m name = withCString name $ FFI.getTypeByName m

getValueName :: Value -> IO String
getValueName v = FFI.getValueName v >>= peekCString

setValueName :: Value -> String -> IO ()
setValueName v name = withCString name $ FFI.setValueName v

addGlobal :: Module -> Type -> String -> IO Value
addGlobal m ty name = withCString name $ FFI.addGlobal m ty

getNamedGlobal :: Module -> String -> IO Value
getNamedGlobal m name = withCString name $ FFI.getNamedGlobal m

buildGlobalString :: Builder -> String -> String -> IO Value
buildGlobalString b string name
    = withCString name (\n -> withCString string (\s -> FFI.buildGlobalString b s n))

buildGlobalStringPtr :: Builder -> String -> String -> IO Value
buildGlobalStringPtr b string name
    = withCString name (\n -> withCString string (\s -> FFI.buildGlobalStringPtr b s n))

addFunction :: Module -> String -> Type -> IO Value
addFunction m name ty = withCString name (\n -> FFI.addFunction m n ty)

getNamedFunction :: Module -> String -> IO Value
getNamedFunction m name = withCString name $ FFI.getNamedFunction m

getParams :: Value -> IO [Value]
getParams f
    = do let count = fromIntegral $ FFI.countParams f
         allocaArray count $ \ptr -> do
           FFI.getParams f ptr
           peekArray count ptr

isTailCall :: Value -> IO Bool
isTailCall call = fmap toBool $ FFI.isTailCall call

setTailCall :: Value -> Bool -> IO ()
setTailCall call isTailCall = FFI.setTailCall call $ fromBool isTailCall

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
      return $ FFI.functionType returnTy ptr (fromIntegral len) (fromBool isVarArg)

-- unsafePerformIO just to wrap the non-effecting withArrayLen call
structType :: [Type] -> Bool -> Type
structType types packed = unsafePerformIO $
    withArrayLen types $ \ len ptr ->
        return $ FFI.structType ptr (fromIntegral len) (fromBool packed)

structCreateNamed :: String -> IO Type
structCreateNamed name
    = do ctx <- FFI.getGlobalContext
         structCreateNamedInContext ctx name

structCreateNamedInContext :: Context -> String -> IO Type
structCreateNamedInContext ctx name = withCString name $ FFI.structCreateNamed ctx

structSetBody :: Type -> [Type] -> Bool -> IO ()
structSetBody struct body packed
    = withArrayLen body $ \len ptr ->
      FFI.structSetBody struct ptr (fromIntegral len) $ fromBool packed

appendBasicBlock :: Value -> String -> IO BasicBlock
appendBasicBlock function name = withCString name $ FFI.appendBasicBlock function

getLinkage :: Value -> IO Linkage
getLinkage v = fmap FFI.toLinkage $ FFI.getLinkage v

setLinkage :: Value -> Linkage -> IO ()
setLinkage v l = FFI.setLinkage v (FFI.fromLinkage l)

constInt :: Type -> CULLong -> Bool -> Value
constInt ty val signExtend = FFI.constInt ty val $ fromBool signExtend

-- unsafePerformIO just to wrap the non-effecting withCStringLen call
constString :: String -> Bool -> Value
constString str dontNullTerminate
    = unsafePerformIO $ withCStringLen str $ \(ptr, len) ->
      return $ FFI.constString ptr (fromIntegral len) $ fromBool dontNullTerminate

withBuilder :: (Builder -> IO a) -> IO a
withBuilder f = do p <- createBuilder
                   finally (f p) (disposeBuilder p)

buildAdd :: Builder -> Value -> Value -> String -> IO Value
buildAdd b x y name = withCString name $ FFI.buildAdd b x y

buildSub :: Builder -> Value -> Value -> String -> IO Value
buildSub b x y name = withCString name $ FFI.buildSub b x y

buildMul :: Builder -> Value -> Value -> String -> IO Value
buildMul b x y name = withCString name $ FFI.buildMul b x y

buildUDiv :: Builder -> Value -> Value -> String -> IO Value
buildUDiv b x y name = withCString name $ FFI.buildUDiv b x y

buildSDiv :: Builder -> Value -> Value -> String -> IO Value
buildSDiv b x y name = withCString name $ FFI.buildSDiv b x y

buildFAdd :: Builder -> Value -> Value -> String -> IO Value
buildFAdd b x y name = withCString name $ FFI.buildFAdd b x y

buildFSub :: Builder -> Value -> Value -> String -> IO Value
buildFSub b x y name = withCString name $ FFI.buildFSub b x y

buildFMul :: Builder -> Value -> Value -> String -> IO Value
buildFMul b x y name = withCString name $ FFI.buildFMul b x y

buildFDiv :: Builder -> Value -> Value -> String -> IO Value
buildFDiv b x y name = withCString name $ FFI.buildFDiv b x y

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
    deriving (Eq, Enum, Show)

fromIntPredicate :: IntPredicate -> CInt
fromIntPredicate p = fromIntegral (fromEnum p + 32)

buildICmp :: Builder -> IntPredicate -> Value -> Value -> String -> IO Value
buildICmp b p l r n = withCString n $ FFI.buildICmp b (fromIntPredicate p) l r

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
    deriving (Eq, Enum, Show)

fromFPPredicate :: FPPredicate -> CInt
fromFPPredicate p = fromIntegral (fromEnum p)

buildFCmp :: Builder -> FPPredicate -> Value -> Value -> String -> IO Value
buildFCmp b p l r n = withCString n $ FFI.buildFCmp b (fromFPPredicate p) l r

buildLoad :: Builder -> Value -> String -> IO Value
buildLoad b ptr name = withCString name $ FFI.buildLoad b ptr

buildStructGEP :: Builder -> Value -> CUInt -> String -> IO Value
buildStructGEP b s idx name = withCString name $ FFI.buildStructGEP b s idx

buildInBoundsGEP :: Builder -> Value -> [Value] -> String -> IO Value
buildInBoundsGEP b ptr indices name
    = withArrayLen indices $ \len indicesPtr ->
      withCString name $ FFI.buildInBoundsGEP b ptr indicesPtr $ fromIntegral len

buildTrunc :: Builder -> Value -> Type -> String -> IO Value
buildTrunc b v t name = withCString name $ FFI.buildTrunc b v t

buildZExt :: Builder -> Value -> Type -> String -> IO Value
buildZExt b v t name = withCString name $ FFI.buildZExt b v t

buildSExt :: Builder -> Value -> Type -> String -> IO Value
buildSExt b v t name = withCString name $ FFI.buildSExt b v t

buildFPToUI :: Builder -> Value -> Type -> String -> IO Value
buildFPToUI b v t name = withCString name $ FFI.buildFPToUI b v t

buildFPToSI :: Builder -> Value -> Type -> String -> IO Value
buildFPToSI b v t name = withCString name $ FFI.buildFPToSI b v t

buildUIToFP :: Builder -> Value -> Type -> String -> IO Value
buildUIToFP b v t name = withCString name $ FFI.buildUIToFP b v t

buildSIToFP :: Builder -> Value -> Type -> String -> IO Value
buildSIToFP b v t name = withCString name $ FFI.buildSIToFP b v t

buildFPTrunc :: Builder -> Value -> Type -> String -> IO Value
buildFPTrunc b v t name = withCString name $ FFI.buildFPTrunc b v t

buildFPExt :: Builder -> Value -> Type -> String -> IO Value
buildFPExt b v t name = withCString name $ FFI.buildFPExt b v t

buildPtrToInt :: Builder -> Value -> Type -> String -> IO Value
buildPtrToInt b v t name = withCString name $ FFI.buildPtrToInt b v t

buildIntToPtr :: Builder -> Value -> Type -> String -> IO Value
buildIntToPtr b v t name = withCString name $ FFI.buildIntToPtr b v t

buildBitCast :: Builder -> Value -> Type -> String -> IO Value
buildBitCast b v t name = withCString name $ FFI.buildBitCast b v t

buildPointerCast :: Builder -> Value -> Type -> String -> IO Value
buildPointerCast b v t name = withCString name $ FFI.buildPointerCast b v t

buildTruncOrBitCast :: Builder -> Value -> Type -> String -> IO Value
buildTruncOrBitCast b v t name = withCString name $ FFI.buildTruncOrBitCast b v t

buildZExtOrBitCast :: Builder -> Value -> Type -> String -> IO Value
buildZExtOrBitCast b v t name = withCString name $ FFI.buildZExtOrBitCast b v t

buildSExtOrBitCast :: Builder -> Value -> Type -> String -> IO Value
buildSExtOrBitCast b v t name = withCString name $ FFI.buildSExtOrBitCast b v t

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

