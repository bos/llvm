{-# LANGUAGE TypeOperators #-}
module HelloJIT (main) where

import Data.Int(Int32)
import Data.Word(Word8)

import Data.TypeNumbers
import LLVM.Core
import LLVM.ExecutionEngine


buildModule :: IO (Module, Function (Int32 -> Int32))
buildModule = do
  m <- newModule "hello"
  func <- defineModule m $ do
    greetz <- createGlobal "hello jit!"
    puts <- externFunction :: CodeGenModule (Function (Ptr Word8 -> Ptr Word8))
    func <- createFunction $ \ _x -> do
      let zero = valueOf (0::Int32)
      tmp <- getElementPtr greetz [zero, zero]
      call puts tmp
      ret zero
    return func
  return (m, func)

execute :: Module -> Function (Int32 -> Int32) -> IO ()
execute m func = do
  prov <- createModuleProviderForExistingModule m
  ee <- createExecutionEngine prov
--  runStaticConstructors ee
  let f = unsafeGeneratePureFunction ee func
  print (f 0)
--  runStaticDestructors ee
  return ()

main :: IO ()
main = buildModule >>= uncurry execute

externFunction :: (IsFunction f) => CodeGenModule (Function f)
externFunction = undefined

createGlobal :: String -> CodeGenModule (Value (Array (D0 End) Word8))
--createGlobal :: (IsTypeNumber n) => String -> CodeGenModule (Value (Array n Word8))
createGlobal _ = undefined
