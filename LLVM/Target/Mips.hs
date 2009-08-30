{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Target.Mips(initializeTarget) where

initializeTarget :: IO ()
initializeTarget = do
    initializeMipsTargetInfo
    initializeMipsTarget

foreign import ccall unsafe "LLVMInitializeMipsTargetInfo" initializeMipsTargetInfo :: IO ()
foreign import ccall unsafe "LLVMInitializeMipsTarget" initializeMipsTarget :: IO ()
