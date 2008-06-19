{-# LANGUAGE TypeOperators #-}
module HelloJIT (main) where

import Data.Int(Int32)
import Data.Word

import LLVM.Core
import LLVM.ExecutionEngine

buildModule :: IO (Module, Function (IO ()))
buildModule = do
  m <- newNamedModule "hello"
  func <- defineModule m $ do
    greetz <- createGlobal InternalLinkage $ constStringNul "Hello, JIT!"
    puts <- newNamedFunction ExternalLinkage "puts" :: TFunction (Ptr Word8 -> IO Word32)
    func <- createFunction ExternalLinkage $ do
      let zero = valueOf (0::Int32)
      tmp <- getElementPtr greetz [zero, zero]
      call puts tmp -- Throw away return value.
      ret ()
    return func
  return (m, func)

execute :: Module -> Function (IO ()) -> IO ()
execute m func = do
  prov <- createModuleProviderForExistingModule m
  ee <- createExecutionEngine prov
  runStaticConstructors ee -- Not needed here.
  let greet = generateFunction ee func
  greet
  greet
  runStaticDestructors ee -- Not needed here.
  return ()

main :: IO ()
main = buildModule >>= uncurry execute
