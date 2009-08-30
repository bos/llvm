{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Target.XCore(initializeTarget) where

initializeTarget :: IO ()
initializeTarget = do
    initializeXCoreTargetInfo
    initializeXCoreTarget

foreign import ccall unsafe "LLVMInitializeXCoreTargetInfo" initializeXCoreTargetInfo :: IO ()
foreign import ccall unsafe "LLVMInitializeXCoreTarget" initializeXCoreTarget :: IO ()
