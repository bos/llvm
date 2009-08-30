{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Target.MSIL(initializeTarget) where

initializeTarget :: IO ()
initializeTarget = do
    initializeMSILTargetInfo
    initializeMSILTarget

foreign import ccall unsafe "LLVMInitializeMSILTargetInfo" initializeMSILTargetInfo :: IO ()
foreign import ccall unsafe "LLVMInitializeMSILTarget" initializeMSILTarget :: IO ()
