{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Target.Alpha(initializeTarget) where

initializeTarget :: IO ()
initializeTarget = do
    initializeAlphaTargetInfo
    initializeAlphaTarget

foreign import ccall unsafe "LLVMInitializeAlphaTargetInfo" initializeAlphaTargetInfo :: IO ()
foreign import ccall unsafe "LLVMInitializeAlphaTarget" initializeAlphaTarget :: IO ()
