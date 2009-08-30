{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Target.MSP430(initializeTarget) where

initializeTarget :: IO ()
initializeTarget = do
    initializeMSP430TargetInfo
    initializeMSP430Target

foreign import ccall unsafe "LLVMInitializeMSP430TargetInfo" initializeMSP430TargetInfo :: IO ()
foreign import ccall unsafe "LLVMInitializeMSP430Target" initializeMSP430Target :: IO ()
