{-# LANGUAGE TypeOperators #-}
module HelloJIT (main) where

import Data.Int(Int32)
import Data.Word

--import Data.TypeNumbers
import LLVM.Core
import LLVM.ExecutionEngine

buildModule :: IO (Module, Function (Word32 -> Word32))
buildModule = do
  m <- newNamedModule "hello"
  func <- defineModule m $ do
    greetz <- createGlobal InternalLinkage $ constStringNul "hello jit!"
    puts <- newNamedFunction ExternalLinkage "puts" :: TFunction (Ptr Word8 -> Word32)
    func <- createFunction ExternalLinkage $ \ _x -> do
      let zero = valueOf (0::Int32)
      tmp <- getElementPtr greetz [zero, zero]
      r <- call puts tmp
      ret r
    return func
  return (m, func)

execute :: Module -> Function (Word32 -> Word32) -> IO ()
execute m func = do
  prov <- createModuleProviderForExistingModule m
  ee <- createExecutionEngine prov
  runStaticConstructors ee
  let f = unsafeGeneratePureFunction ee func
  print (f 0)
  runStaticDestructors ee
  return ()

main :: IO ()
main = buildModule >>= uncurry execute
