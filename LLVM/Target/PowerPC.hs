{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Target.PowerPC(initializeTarget) where

initializeTarget :: IO ()
initializeTarget = do
    initializePowerPCTargetInfo
    initializePowerPCTarget

foreign import ccall unsafe "LLVMInitializePowerPCTargetInfo" initializePowerPCTargetInfo :: IO ()
foreign import ccall unsafe "LLVMInitializePowerPCTarget" initializePowerPCTarget :: IO ()
