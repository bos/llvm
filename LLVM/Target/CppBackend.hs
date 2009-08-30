{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Target.CppBackend(initializeTarget) where

initializeTarget :: IO ()
initializeTarget = do
    initializeCppBackendTargetInfo
    initializeCppBackendTarget

foreign import ccall unsafe "LLVMInitializeCppBackendTargetInfo" initializeCppBackendTargetInfo :: IO ()
foreign import ccall unsafe "LLVMInitializeCppBackendTarget" initializeCppBackendTarget :: IO ()
