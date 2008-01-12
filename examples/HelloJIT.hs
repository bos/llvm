{-# LANGUAGE TypeOperators #-}
module HelloJIT (main) where

import Data.Int(Int32)
import Data.Word

--import Data.TypeNumbers
import LLVM.Core
import LLVM.ExecutionEngine

buildModule :: IO (Module, Function (IO ()))
buildModule = do
  m <- newNamedModule "hello"
  func <- defineModule m $ do
    greetz <- createGlobal InternalLinkage $ constStringNul "hello jit!"
    puts <- newNamedFunction ExternalLinkage "puts" :: TFunction (Ptr Word8 -> IO Word32)
    func <- createFunction ExternalLinkage $ do
      let zero = valueOf (0::Int32)
      tmp <- getElementPtr greetz [zero, zero]
      _r <- call puts tmp
      ret ()
    return func
  return (m, func)

execute :: Module -> Function (IO ()) -> IO ()
execute m func = do
  prov <- createModuleProviderForExistingModule m
  ee <- createExecutionEngine prov
  runStaticConstructors ee
  let fun = generateFunction ee func
  fun
  runStaticDestructors ee
  return ()

main :: IO ()
main = buildModule >>= uncurry execute
