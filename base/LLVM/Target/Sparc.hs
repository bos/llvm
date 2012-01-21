{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Target.Sparc(initializeTarget) where

initializeTarget :: IO ()
initializeTarget = do
    initializeSparcTargetInfo
    initializeSparcTarget

foreign import ccall unsafe "LLVMInitializeSparcTargetInfo" initializeSparcTargetInfo :: IO ()
foreign import ccall unsafe "LLVMInitializeSparcTarget" initializeSparcTarget :: IO ()
