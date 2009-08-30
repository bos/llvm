{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Target.SystemZ(initializeTarget) where

initializeTarget :: IO ()
initializeTarget = do
    initializeSystemZTargetInfo
    initializeSystemZTarget

foreign import ccall unsafe "LLVMInitializeSystemZTargetInfo" initializeSystemZTargetInfo :: IO ()
foreign import ccall unsafe "LLVMInitializeSystemZTarget" initializeSystemZTarget :: IO ()
