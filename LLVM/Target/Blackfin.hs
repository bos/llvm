{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Target.Blackfin(initializeTarget) where

initializeTarget :: IO ()
initializeTarget = do
    initializeBlackfinTargetInfo
    initializeBlackfinTarget

foreign import ccall unsafe "LLVMInitializeBlackfinTargetInfo" initializeBlackfinTargetInfo :: IO ()
foreign import ccall unsafe "LLVMInitializeBlackfinTarget" initializeBlackfinTarget :: IO ()
