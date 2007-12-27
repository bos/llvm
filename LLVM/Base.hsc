{-# LANGUAGE EmptyDataDecls #-}

module LLVM.Base
    (
      Module
    , moduleCreateWithName
    , disposeModule

    , Type
    , addTypeName
    , deleteTypeName
    , getElementType

    , Value
    , addGlobal
    , deleteGlobal
    , setInitializer
    , typeOf
    , getNamedFunction
    ) where

import Foreign.C.String (CString)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr)


data Module

foreign import ccall unsafe "LLVMModuleCreateWithName" moduleCreateWithName
    :: CString -> IO (Ptr Module)

foreign import ccall unsafe "LLVMDisposeModule" disposeModule
    :: Ptr Module -> IO ()


data Type

foreign import ccall unsafe "LLVMAddTypeName" addTypeName
    :: Ptr Module -> CString -> Ptr Type -> IO CInt

foreign import ccall unsafe "LLVMDeleteTypeName" deleteTypeName
    :: Ptr Module -> CString -> IO ()

foreign import ccall unsafe "LLVMGetElementType" getElementType
    :: Ptr Type -> IO (Ptr Type)


data Value

foreign import ccall unsafe "LLVMAddGlobal" addGlobal
    :: Ptr Module -> Ptr Type -> CString -> IO (Ptr Value)

foreign import ccall unsafe "LLVMDeleteGlobal" deleteGlobal
    :: Ptr Value -> IO ()

foreign import ccall unsafe "LLVMSetInitializer" setInitializer
    :: Ptr Value -> Ptr Value -> IO ()

foreign import ccall unsafe "LLVMTypeOf" typeOf
    :: Ptr Value -> IO (Ptr Type)

foreign import ccall unsafe "LLVMGetNamedFunction" getNamedFunction
    :: CString -> IO (Ptr Value)
