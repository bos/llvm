module LLVM
    (
      Module
    , createModule

    , Type
    , addTypeName
    , deleteTypeName

    , Value
    , addGlobal
    ) where

import Control.Applicative ((<$>))
import Foreign.C.String (withCString)
import Foreign.ForeignPtr (ForeignPtr, FinalizerPtr, newForeignPtr,
                           withForeignPtr)
import Foreign.Ptr (Ptr)

import qualified LLVM.Base as Base


newtype Module = Module (ForeignPtr Base.Module)

createModule :: String -> IO Module
createModule name =
    withCString name $ \namePtr -> do
      ptr <- Base.moduleCreateWithName namePtr
      final <- h2c_module Base.disposeModule
      Module <$> newForeignPtr final ptr

foreign import ccall "wrapper" h2c_module
    :: (Ptr Base.Module -> IO ()) -> IO (FinalizerPtr a)


newtype Type = Type (ForeignPtr Base.Type)

addGlobal :: Module -> Type -> String -> IO Value
addGlobal (Module mod) (Type typ) name =
    withForeignPtr mod $ \modPtr ->
      withForeignPtr typ $ \typPtr ->
        withCString name $ \namePtr -> do
          ptr <- Base.addGlobal modPtr typPtr namePtr
          final <- h2c_value Base.deleteGlobal
          Value <$> newForeignPtr final ptr

addTypeName :: Module -> Type -> String -> IO Bool
addTypeName (Module mod) (Type typ) name =
    withForeignPtr mod $ \modPtr ->
      withForeignPtr typ $ \typPtr ->
        withCString name $ \namePtr ->
          (/=0) <$> Base.addTypeName modPtr namePtr typPtr
                 
foreign import ccall "wrapper" h2c_value
    :: (Ptr Base.Value -> IO ()) -> IO (FinalizerPtr a)

deleteTypeName :: Module -> String -> IO ()
deleteTypeName (Module mod) name =
    withForeignPtr mod $ \modPtr ->
      withCString name $ \namePtr ->
        Base.deleteTypeName modPtr namePtr


newtype Value = Value (ForeignPtr Base.Value)
