module Varargs (main) where

import Data.Word

import LLVM.Core
import LLVM.ExecutionEngine

bldVarargs :: CodeGenModule (Function (Word32 -> IO ()))
bldVarargs = do
    printf <- newNamedFunction ExternalLinkage "printf" :: TFunction (Ptr Word8 -> VarArgs Word32)
    fmt1 <- createStringNul "Hello\n"
    fmt2 <- createStringNul "A number %d\n"
    fmt3 <- createStringNul "Two numbers %d %d\n"
    func <- createFunction ExternalLinkage $ \ x -> do

      tmp1 <- getElementPtr0 fmt1 (0::Word32, ())
      let p1 = castVarArgs printf :: Function (Ptr Word8 -> IO Word32)
      _ <- call p1 tmp1

      tmp2 <- getElementPtr0 fmt2 (0::Word32, ())
      let p2 = castVarArgs printf :: Function (Ptr Word8 -> Word32 -> IO Word32)
      _ <- call p2 tmp2 x

      tmp3 <- getElementPtr0 fmt3 (0::Word32, ())
      let p3 = castVarArgs printf :: Function (Ptr Word8 -> Word32 -> Word32 -> IO Word32)
      _ <- call p3 tmp3 x x

      ret ()
    return func

main :: IO ()
main = do
    initializeNativeTarget
    varargs <- simpleFunction bldVarargs
    varargs 42
    return ()
