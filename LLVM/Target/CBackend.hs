{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Target.CBackend(initializeTarget) where

initializeTarget :: IO ()
initializeTarget = do
    initializeCBackendTargetInfo
    initializeCBackendTarget

foreign import ccall unsafe "LLVMInitializeCBackendTargetInfo" initializeCBackendTargetInfo :: IO ()
foreign import ccall unsafe "LLVMInitializeCBackendTarget" initializeCBackendTarget :: IO ()
