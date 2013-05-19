module LLVM.Wrapper.Transforms.PassManagerBuilder where

import Foreign.C.Types
import Foreign.ForeignPtr.Safe (withForeignPtr)
import Foreign.Marshal.Utils (fromBool, toBool)

import qualified LLVM.FFI.Transforms.PassManagerBuilder as FFI
import LLVM.Wrapper.Internal

passManagerBuilderCreate :: IO PassManagerBuilder
passManagerBuilderCreate = initPassManagerBuilder =<< FFI.passManagerBuilderCreate

passManagerBuilderSetOptLevel :: PassManagerBuilder -> CUInt -> IO ()
passManagerBuilderSetOptLevel (MkPassManagerBuilder p) level =
    withForeignPtr p (`FFI.passManagerBuilderSetOptLevel` level)

passManagerBuilderSetSizeLevel :: PassManagerBuilder -> CUInt -> IO ()
passManagerBuilderSetSizeLevel (MkPassManagerBuilder p) level =
    withForeignPtr p (`FFI.passManagerBuilderSetSizeLevel` level)

passManagerBuilderPopulateFunctionPassManager :: PassManagerBuilder -> PassManager -> IO ()
passManagerBuilderPopulateFunctionPassManager (MkPassManagerBuilder b) (MkPassManager m) =
    withForeignPtr b $ \bptr ->
    withForeignPtr m $ \mptr ->
        FFI.passManagerBuilderPopulateFunctionPassManager bptr mptr

passManagerBuilderPopulateModulePassManager :: PassManagerBuilder -> PassManager -> IO ()
passManagerBuilderPopulateModulePassManager (MkPassManagerBuilder b) (MkPassManager m) =
    withForeignPtr b $ \bptr ->
    withForeignPtr m $ \mptr ->
        FFI.passManagerBuilderPopulateModulePassManager bptr mptr

passManagerBuilderPopulateLTOPassManager :: PassManagerBuilder -> PassManager -> Bool -> Bool -> IO ()
passManagerBuilderPopulateLTOPassManager (MkPassManagerBuilder b) (MkPassManager m) internalize inline =
    withForeignPtr b $ \bptr ->
    withForeignPtr m $ \mptr ->
        FFI.passManagerBuilderPopulateLTOPassManager bptr mptr (fromBool internalize) (fromBool inline)

