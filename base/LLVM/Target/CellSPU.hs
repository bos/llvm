{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Target.CellSPU(initializeTarget) where

initializeTarget :: IO ()
initializeTarget = do
    initializeCellSPUTargetInfo
    initializeCellSPUTarget

foreign import ccall unsafe "LLVMInitializeCellSPUTargetInfo" initializeCellSPUTargetInfo :: IO ()
foreign import ccall unsafe "LLVMInitializeCellSPUTarget" initializeCellSPUTarget :: IO ()
