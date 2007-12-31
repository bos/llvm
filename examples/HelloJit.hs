{-# LANGUAGE TypeOperators #-}
module HelloJit (main) where

import Data.Int (Int32)
import LLVM.Core.Types ((:->))
import qualified LLVM.Core as Core
import qualified LLVM.Core.Instructions as I
import qualified LLVM.Core.Types as T
import qualified LLVM.Core.Values as V
import qualified LLVM.ExecutionEngine as EE
import Prelude hiding (mod)

defineGlobal :: (V.ConstValue a, V.TypedValue a t) => T.Module -> String -> a -> IO (V.GlobalVar t)
defineGlobal mod name val = do
  print "foo"
  global <- Core.addGlobal mod (V.typeOf val) name
  print "bar"
  Core.setInitializer global val
  return global

declareFunction :: T.Params p => T.Module -> String -> T.Function p -> IO (V.Function p)
declareFunction mod name typ = do
  maybeFunc <- Core.getNamedFunction mod name
  case maybeFunc of
    Nothing -> Core.addFunction mod name typ
    Just func -> return $ let t = V.typeOf func
                          in if T.elementTypeDyn t /= T.toAnyType typ
                             then I.bitCast func (T.pointer typ)
                             else func

defineFunction :: T.Params p => T.Module -> String -> T.Function p
               -> IO (V.Function p, V.BasicBlock)
defineFunction mod name typ = do
  func <- Core.addFunction mod name typ
  bblk <- Core.appendBasicBlock func "entry"
  return (func, bblk)

buildModule :: IO (T.Module, V.Function T.Int32)
buildModule = do
  mod <- Core.createModule "hello"
  greetz <- defineGlobal mod "greeting" (V.const "hello jit!")
  let t = undefined :: T.Pointer T.Int8 :-> T.Int32
  putStrLn $ "type of puts: " ++ show t
  puts <- declareFunction mod "puts" (T.function t)
  (func, entry) <- defineFunction mod "main"
                   (T.function (undefined :: T.Int32))
  bld <- Core.createBuilder
  Core.positionAtEnd bld entry
  let zero = V.const (0::Int32)
  tmp <- Core.buildGEP bld greetz [zero, zero] "tmp"
  Core.buildCall bld puts [V.mkAnyValue tmp] ""
  Core.buildRet bld zero
  return (mod, func)

execute :: T.Module -> V.Function a -> IO ()
execute mod func = do
  prov <- Core.createModuleProviderForExistingModule mod
  ee <- EE.createExecutionEngine prov
  EE.runStaticConstructors ee
  EE.runFunction ee func []
  EE.runStaticDestructors ee
  return ()

main :: IO ()
main = buildModule >>= uncurry execute
