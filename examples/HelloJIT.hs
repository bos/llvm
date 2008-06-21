module HelloJIT (main) where

import Data.Int(Int32)
import Data.Word

import LLVM.Core
import LLVM.ExecutionEngine

bldGreet :: CodeGenModule (Function (IO ()))
bldGreet = do
    greetz <- createGlobal InternalLinkage $ constStringNul "Hello, JIT!"
    puts <- newNamedFunction ExternalLinkage "puts" :: TFunction (Ptr Word8 -> IO Word32)
    func <- createFunction ExternalLinkage $ do
      let zero = valueOf (0::Int32)
      tmp <- getElementPtr greetz [zero::Value Int32, zero]
      call puts tmp -- Throw away return value.
      ret ()
    return func

main :: IO ()
main = do
  greet <- simpleFunction bldGreet
  greet
  greet
  return ()
