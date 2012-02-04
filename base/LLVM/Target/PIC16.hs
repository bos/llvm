{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Target.PIC16(initializeTarget) where

initializeTarget :: IO ()
initializeTarget = do
    initializePIC16TargetInfo
    initializePIC16Target

foreign import ccall unsafe "LLVMInitializePIC16TargetInfo" initializePIC16TargetInfo :: IO ()
foreign import ccall unsafe "LLVMInitializePIC16Target" initializePIC16Target :: IO ()
