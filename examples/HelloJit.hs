{-# LANGUAGE TypeOperators #-}
module HelloJit (main) where

import Data.Int (Int32)
import LLVM.Core.Types ((:->))
import qualified LLVM.Core as Core
import qualified LLVM.Core.Types as T
import qualified LLVM.ExecutionEngine as EE
import Prelude hiding (mod)

defineGlobal :: T.Module -> String -> T.Value -> IO T.Value
defineGlobal mod name val = do
  global <- Core.addGlobal mod (Core.typeOf val) name
  Core.setInitializer global val
  return global

declareFunction :: T.Params p => T.Module -> String -> T.Function p -> IO T.Value
declareFunction mod name typ = do
  maybeFunc <- Core.getNamedFunction mod name
  case maybeFunc of
    Nothing -> Core.addFunction mod name typ
    Just func -> return $ let t = T.fromAny . Core.typeOf $ func :: T.Pointer a
                          in if Core.getElementType t /= T.toAny typ
                             then Core.constBitCast (Core.pointerType typ) func
                             else func

defineFunction :: T.Params p => T.Module -> String -> T.Function p
               -> IO (T.Value, T.BasicBlock)
defineFunction mod name typ = do
  func <- Core.addFunction mod name typ
  bblk <- Core.appendBasicBlock func "entry"
  return (func, bblk)

buildModule :: IO (T.Module, T.Value)
buildModule = do
  mod <- Core.createModule "hello"
  greetz <- defineGlobal mod "greeting" (Core.const "hello jit!")
  let t = undefined :: T.Pointer T.Int8 :-> T.Int32
  putStrLn $ "type of puts: " ++ show t
  puts <- declareFunction mod "puts" (Core.functionType t)
  (func, entry) <- defineFunction mod "main"
                   (Core.functionType (undefined :: T.Int32))
  bld <- Core.createBuilder
  Core.positionAtEnd bld entry
  let zero = Core.const (0::Int32)
  tmp <- Core.buildGEP bld greetz [zero, zero] "tmp"
  Core.buildCall bld puts [tmp] ""
  Core.buildRet bld zero
  return (mod, func)

execute :: T.Module -> T.Value -> IO ()
execute mod func = do
  prov <- Core.createModuleProviderForExistingModule mod
  ee <- EE.createExecutionEngine prov
  EE.runStaticConstructors ee
  EE.runFunction ee func []
  EE.runStaticDestructors ee
  return ()

main :: IO ()
main = buildModule >>= uncurry execute
