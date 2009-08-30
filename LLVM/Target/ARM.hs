{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Target.ARM(initializeTarget) where

initializeTarget :: IO ()
initializeTarget = do
    initializeARMTargetInfo
    initializeARMTarget

foreign import ccall unsafe "LLVMInitializeARMTargetInfo" initializeARMTargetInfo :: IO ()
foreign import ccall unsafe "LLVMInitializeARMTarget" initializeARMTarget :: IO ()
