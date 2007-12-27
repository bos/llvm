{-# LANGUAGE TypeSynonymInstances #-}

module LLVM
    (
      Module
    , createModule

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

    -- ** Operations on array, pointer, and vector types
    , pointerType

    , Value
    , addGlobal
    , setInitializer
    , typeOf

    -- ** Operations on functions
    , addFunction
    , deleteFunction
    , getNamedFunction

    -- ** Operations on scalar constants
    , Const(..)
    , constInt
    , constWord
    , constReal

    -- ** Operations on composite constants
    , constString
    , constStringNul

    -- ** Constant expressions
    , constBitCast

    -- ** Basic blocks
    , BasicBlock

    -- * Instruction building
    , Builder
    , createBuilder
    , positionBefore
    , positionAtEnd
    ) where

import Control.Applicative ((<$>))
import Data.Int (Int64)
import Data.Word (Word64)
import Foreign.C.String (withCString, withCStringLen)
import Foreign.ForeignPtr (ForeignPtr, FinalizerPtr, newForeignPtr,
                           withForeignPtr)
import Foreign.Ptr (Ptr, nullPtr)
import System.IO.Unsafe (unsafePerformIO)

import qualified LLVM.Base as Base


newtype Module = Module {fromModule :: ForeignPtr Base.Module}

withModule :: Module -> (Ptr Base.Module -> IO a) -> IO a
withModule mod = withForeignPtr (fromModule mod)

createModule :: String -> IO Module
createModule name =
    withCString name $ \namePtr -> do
      ptr <- Base.moduleCreateWithName namePtr
      final <- h2c_module Base.disposeModule
      Module <$> newForeignPtr final ptr

foreign import ccall "wrapper" h2c_module
    :: (Ptr Base.Module -> IO ()) -> IO (FinalizerPtr a)


newtype Type = Type {fromType :: Ptr Base.Type}

instance Eq Type where
    a == b = fromType a == fromType b

addTypeName :: Module -> Type -> String -> IO Bool
addTypeName mod typ name =
    withModule mod $ \modPtr ->
      withCString name $ \namePtr ->
        (/=0) <$> Base.addTypeName modPtr namePtr (fromType typ)
                 
deleteTypeName :: Module -> String -> IO ()
deleteTypeName mod name =
    withModule mod $ \modPtr ->
      withCString name $ Base.deleteTypeName modPtr

getElementType :: Type -> Type
getElementType typ = unsafePerformIO $
    Type <$> Base.getElementType (fromType typ)

int1Type :: Type
int1Type = unsafePerformIO $ Type <$> Base.int1Type

int8Type :: Type
int8Type = unsafePerformIO $ Type <$> Base.int8Type

int16Type :: Type
int16Type = unsafePerformIO $ Type <$> Base.int16Type

int32Type :: Type
int32Type = unsafePerformIO $ Type <$> Base.int32Type

int64Type :: Type
int64Type = unsafePerformIO $ Type <$> Base.int64Type

integerType :: Int -> Type
integerType width = unsafePerformIO $
    Type <$> Base.integerType (fromIntegral width)

floatType :: Type
floatType = unsafePerformIO $ Type <$> Base.floatType

doubleType :: Type
doubleType = unsafePerformIO $ Type <$> Base.doubleType

x86FP80Type :: Type
x86FP80Type = unsafePerformIO $ Type <$> Base.x86FP80Type

fp128Type :: Type
fp128Type = unsafePerformIO $ Type <$> Base.fp128Type

ppcFP128Type :: Type
ppcFP128Type = unsafePerformIO $ Type <$> Base.ppcFP128Type

pointerType :: Type -> Int -> Type
pointerType typ addressSpace = unsafePerformIO $
    Type <$> Base.pointerType (fromType typ) (fromIntegral addressSpace)

newtype Value = Value {fromValue :: Ptr Base.Value}

addGlobal :: Module -> Type -> String -> IO Value
addGlobal mod typ name =
    withModule mod $ \modPtr ->
      withCString name $ \namePtr ->
        Value <$> Base.addGlobal modPtr (fromType typ) namePtr

setInitializer :: Value -> Value -> IO ()
setInitializer global const =
    Base.setInitializer (fromValue global) (fromValue const)

typeOf :: Value -> Type
typeOf val = unsafePerformIO $ Type <$> Base.typeOf (fromValue val)

addFunction :: Module -> Type -> String -> IO Value
addFunction mod typ name =
    withModule mod $ \modPtr ->
      withCString name $ \namePtr ->
        Value <$> Base.addFunction modPtr namePtr (fromType typ)

deleteFunction :: Value -> IO ()
deleteFunction = Base.deleteFunction . fromValue

maybePtr :: (Ptr a -> b) -> Ptr a -> Maybe b
maybePtr f ptr | ptr /= nullPtr = Just (f ptr)
               | otherwise = Nothing

getNamedFunction :: String -> IO (Maybe Value)
getNamedFunction name = withCString name $ \namePtr ->
    maybePtr Value <$> Base.getNamedFunction namePtr

constWord :: Type -> Word64 -> Value
constWord typ val = unsafePerformIO $
    Value <$> Base.constInt (fromType typ) (fromIntegral val) 0

constInt :: Type -> Int64 -> Value
constInt typ val = unsafePerformIO $
    Value <$> Base.constInt (fromType typ) (fromIntegral val) 1

constReal :: Type -> Double -> Value
constReal typ val = unsafePerformIO $
    Value <$> Base.constReal (fromType typ) (realToFrac val)

constString :: String -> Value
constString s = unsafePerformIO $
    withCStringLen s $ \(sPtr, sLen) ->
      Value <$> Base.constString sPtr (fromIntegral sLen) 1

constStringNul :: String -> Value
constStringNul s = unsafePerformIO $
    withCStringLen s $ \(sPtr, sLen) ->
      Value <$> Base.constString sPtr (fromIntegral sLen) 0

constBitCast :: Type -> Value -> Value
constBitCast typ val = unsafePerformIO $
    Value <$> Base.constBitCast (fromValue val) (fromType typ)

class Const a where
    const :: a -> Value

instance Const String where
    const = constStringNul


newtype BasicBlock = BasicBlock {fromBasicBlock :: Ptr Base.BasicBlock}


newtype Builder = Builder {fromBuilder :: ForeignPtr Base.Builder}

withBuilder :: Builder -> (Ptr Base.Builder -> IO a) -> IO a
withBuilder bld = withForeignPtr (fromBuilder bld)

createBuilder :: IO Builder
createBuilder = do
  final <- h2c_builder Base.disposeBuilder
  ptr <- Base.createBuilder
  Builder <$> newForeignPtr final ptr

foreign import ccall "wrapper" h2c_builder
    :: (Ptr Base.Builder -> IO ()) -> IO (FinalizerPtr a)

positionBefore :: Builder -> Value -> IO ()
positionBefore bld insn =
    withBuilder bld $ \bldPtr ->
      Base.positionBefore bldPtr (fromValue insn)

positionAtEnd :: Builder -> BasicBlock -> IO ()
positionAtEnd bld bblk =
    withBuilder bld $ \bldPtr ->
      Base.positionAtEnd bldPtr (fromBasicBlock bblk)
