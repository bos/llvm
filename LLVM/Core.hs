{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module LLVM.Core
    (
    -- * Modules
      createModule

    -- * Module providers
    , createModuleProviderForExistingModule

    -- * Types
    , addTypeName
    , deleteTypeName

    -- * Values
    , addGlobal
    , setInitializer

    -- ** Operations on functions
    , addFunction
    , deleteFunction
    , getNamedFunction

    -- * Basic blocks
    , appendBasicBlock
    , insertBasicBlock
    , deleteBasicBlock
    ) where

import Control.Applicative ((<$>))
import Foreign.C.String (withCString)
import Foreign.Marshal.Utils (toBool)
import Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import Foreign.Ptr (Ptr, nullPtr)
import Prelude hiding (mod)

import qualified LLVM.Core.FFI as FFI
import qualified LLVM.Core.Builders as B
import qualified LLVM.Core.Types as T
import qualified LLVM.Core.Values as V


createModule :: String -> IO T.Module
createModule name =
    withCString name $ \namePtr -> do
      ptr <- FFI.moduleCreateWithName namePtr
      final <- h2c_module FFI.disposeModule
      T.Module <$> newForeignPtr final ptr

foreign import ccall "wrapper" h2c_module
    :: (FFI.ModuleRef -> IO ()) -> IO (FinalizerPtr a)


createModuleProviderForExistingModule :: T.Module -> IO T.ModuleProvider
createModuleProviderForExistingModule mod =
    T.withModule mod $ \modPtr -> do
        ptr <- FFI.createModuleProviderForExistingModule modPtr
        final <- h2c_moduleProvider FFI.disposeModuleProvider
        T.ModuleProvider <$> newForeignPtr final ptr

foreign import ccall "wrapper" h2c_moduleProvider
    :: (FFI.ModuleProviderRef -> IO ()) -> IO (FinalizerPtr a)


addTypeName :: (T.Type t) => T.Module -> t -> String -> IO Bool
addTypeName mod typ name =
    T.withModule mod $ \modPtr ->
      withCString name $ \namePtr ->
        toBool <$> FFI.addTypeName modPtr namePtr (T.typeRef typ)
                 
deleteTypeName :: T.Module -> String -> IO ()
deleteTypeName mod name =
    T.withModule mod $ \modPtr ->
      withCString name $ FFI.deleteTypeName modPtr

addGlobal :: (T.Type t) => T.Module -> t -> String -> IO (V.GlobalVar t)
addGlobal mod typ name =
    T.withModule mod $ \modPtr ->
      withCString name $ \namePtr ->
        V.GlobalVar . V.mkAnyValue <$> FFI.addGlobal modPtr (T.typeRef typ) namePtr

setInitializer :: V.ConstValue t => V.GlobalVar a -> t -> IO ()
setInitializer global cnst =
    FFI.setInitializer (V.valueRef global) (V.valueRef cnst)

addFunction :: (T.Params p) => T.Module -> String -> T.Function p
            -> IO (V.Function p)
addFunction mod name typ =
    T.withModule mod $ \modPtr ->
      withCString name $ \namePtr ->
        V.Function . V.mkAnyValue <$> FFI.addFunction modPtr namePtr (T.typeRef typ)

deleteFunction :: V.Function a -> IO ()
deleteFunction = FFI.deleteFunction . V.valueRef

maybePtr :: (Ptr a -> b) -> Ptr a -> Maybe b
maybePtr f ptr | ptr /= nullPtr = Just (f ptr)
               | otherwise = Nothing

getNamedFunction :: T.Module -> String -> IO (Maybe (V.Function a))
getNamedFunction mod name =
    T.withModule mod $ \modPtr ->
      withCString name $ \namePtr ->
        maybePtr (V.Function . V.mkAnyValue) <$> FFI.getNamedFunction modPtr namePtr

appendBasicBlock :: V.Function a -> String -> IO B.BasicBlock
appendBasicBlock func name =
    withCString name $ \namePtr ->
      B.BasicBlock . V.mkAnyValue <$> FFI.appendBasicBlock (V.valueRef func) namePtr

insertBasicBlock :: B.BasicBlock -> String -> IO B.BasicBlock
insertBasicBlock before name =
    withCString name $ \namePtr ->
      B.BasicBlock . V.mkAnyValue <$> FFI.insertBasicBlock (V.valueRef before) namePtr

deleteBasicBlock :: B.BasicBlock -> IO ()
deleteBasicBlock = FFI.deleteBasicBlock . V.valueRef
