module LLVM.Wrapper.Core
    (
    -- ** Modules
      Module
    , moduleCreateWithName
    , disposeModule
    , withModule

    -- * Types
    , Type
    , TypeKind(..)
    , getTypeKind

    -- * Integer types
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

    -- ** Struct types
    , structType

    -- ** Other types
    , pointerType

    -- * Values
    , Value
    , typeOf
    , getValueName
    , setValueName
    , dumpValue
    -- ** Constants
    , constNull
    , constPointerNull
    -- ** Globals
    , addGlobal
    , getNamedGlobal
    -- ** Functions
    , addFunction
    , getNamedFunction
    , getParams

    -- * Basic blocks
    , BasicBlock
    , appendBasicBlock

    -- * Instruction building
    , Builder
    , withBuilder
    , createBuilder
    , disposeBuilder
    , positionAtEnd

    -- ** Terminators
    , buildRetVoid
    , buildRet
    , buildBr
    , buildIndirectBr
    , buildCondBr
    , buildSwitch
    , addCase

    -- ** Memory
    , buildStructGEP

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

    , pointerType
      
    , typeOf
    , dumpValue
    , constNull
    , constPointerNull

    , createBuilder
    , disposeBuilder
    , positionAtEnd

    , buildRetVoid
    , buildRet
    , buildBr
    , buildIndirectBr
    , buildCondBr
    , buildSwitch
    , addCase
    )
import qualified LLVM.FFI.Core as FFI

type Type       = FFI.TypeRef
type Module     = FFI.ModuleRef
type Value      = FFI.ValueRef
type Builder    = FFI.BuilderRef
type BasicBlock = FFI.BasicBlockRef

moduleCreateWithName :: String -> IO Module
moduleCreateWithName name = withCString name FFI.moduleCreateWithName

withModule :: String -> (Module -> IO a) -> IO a
withModule n f = do m <- moduleCreateWithName n
                    finally (f m) (disposeModule m)

getValueName :: Value -> IO String
getValueName v = FFI.getValueName v >>= peekCString

setValueName :: Value -> String -> IO ()
setValueName v name = withCString name $ FFI.setValueName v

addGlobal :: Module -> Type -> String -> IO Value
addGlobal m ty name = withCString name $ FFI.addGlobal m ty

getNamedGlobal :: Module -> String -> IO Value
getNamedGlobal m name = withCString name $ FFI.getNamedGlobal m

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

appendBasicBlock :: Value -> String -> IO BasicBlock
appendBasicBlock function name = withCString name $ FFI.appendBasicBlock function

withBuilder :: (Builder -> IO a) -> IO a
withBuilder f = do p <- createBuilder
                   finally (f p) (disposeBuilder p)

buildStructGEP :: Builder -> Value -> CUInt -> String -> IO Value
buildStructGEP b s idx name = withCString name $ FFI.buildStructGEP b s idx

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

