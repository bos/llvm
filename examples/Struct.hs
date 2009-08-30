{-# LANGUAGE ForeignFunctionInterface, TypeOperators, ScopedTypeVariables #-}
module Struct (main) where

import Data.Word
import Data.TypeLevel(d0, d1, d2, D10)

import LLVM.Core
import LLVM.Util.File
import LLVM.ExecutionEngine

foreign import ccall structCheck :: Word32 -> Ptr S -> Int

-- Watch out for double!  Alignment differs between platforms.
-- struct S { uint32 x0; float x1; uint32 x2[10] };
type S = Struct (Word32 :& Float :& Array D10 Word32 :& ())

-- S *s = malloc(sizeof *s); s->x0 = a; s->x1 = 1.2; s->x2[5] = a+1; return s;
mStruct :: CodeGenModule (Function (Word32 -> IO (Ptr S)))
mStruct = do
    createFunction ExternalLinkage $ \ x -> do
      p  :: Value (Ptr S)
         <- malloc
      p0 <- getElementPtr0 p (d0 & ())
      store x (p0 :: Value (Ptr Word32))
      p1 <- getElementPtr0 p (d1 & ())
      store (valueOf 1.5) p1
      x' <- add x (1 :: Word32)
      p2 <- getElementPtr0 p (d2 & (5::Word32) & ())
      store x' p2
      ret p

main :: IO ()
main = do
    initializeNativeTarget
    writeCodeGenModule "Struct.bc" mStruct
    struct <- simpleFunction mStruct
    let a = 10
    p <- struct a
    putStrLn $ if structCheck a p /= 0 then "OK" else "failed"
    return ()
