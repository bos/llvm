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

    , Value
    , addGlobal
    , setInitializer
    , typeOf
    , getNamedFunction
    ) where

import Control.Applicative ((<$>))
import Foreign.C.String (withCString)
import Foreign.ForeignPtr (ForeignPtr, FinalizerPtr, newForeignPtr,
                           withForeignPtr)
import Foreign.Ptr (Ptr)
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

getNamedFunction :: String -> IO Value
getNamedFunction name = withCString name $ fmap Value . Base.getNamedFunction
