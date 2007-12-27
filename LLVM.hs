module LLVM
    (
      Module
    , createModule

    , Type
    , addTypeName
    , deleteTypeName
    , getElementType

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

getElementType :: Type -> IO Type
getElementType typ = Type <$> Base.getElementType (fromType typ)


newtype Value = Value {fromValue :: Ptr Base.Value}

addGlobal :: Module -> Type -> String -> IO Value
addGlobal mod typ name =
    withModule mod $ \modPtr ->
      withCString name $ \namePtr ->
        Value <$> Base.addGlobal modPtr (fromType typ) namePtr

setInitializer :: Value -> Value -> IO ()
setInitializer global const =
    Base.setInitializer (fromValue global) (fromValue const)

typeOf :: Value -> IO Type
typeOf val = Type <$> Base.typeOf (fromValue val)

getNamedFunction :: String -> IO Value
getNamedFunction name = withCString name $ fmap Value . Base.getNamedFunction
