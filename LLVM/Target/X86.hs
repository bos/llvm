{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Target.X86(initializeTarget) where

initializeTarget :: IO ()
initializeTarget = do
    initializeX86TargetInfo
    initializeX86Target

foreign import ccall unsafe "LLVMInitializeX86TargetInfo" initializeX86TargetInfo :: IO ()
foreign import ccall unsafe "LLVMInitializeX86Target" initializeX86Target :: IO ()
