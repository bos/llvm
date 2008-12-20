module HelloJIT (main) where

import Data.Word

import LLVM.Core
import LLVM.ExecutionEngine

bldGreet :: CodeGenModule (Function (IO ()))
bldGreet = do
    puts <- newNamedFunction ExternalLinkage "puts" :: TFunction (Ptr Word8 -> IO Word32)
    greetz <- createStringNul "Hello, JIT!"
    func <- createFunction ExternalLinkage $ do
      tmp <- getElementPtr greetz (0::Word32, (0::Word32, ()))
      call puts tmp -- Throw away return value.
      ret ()
    return func

main :: IO ()
main = do
    greet <- simpleFunction bldGreet
    greet
    greet
    return ()
