module HelloJit (main) where

import Data.Int (Int32)
import qualified LLVM.Core as Core
import qualified LLVM.ExecutionEngine as EE
import Prelude hiding (mod)

defineGlobal :: Core.Module -> String -> Core.Value -> IO Core.Value
defineGlobal mod name val = do
  global <- Core.addGlobal mod (Core.typeOf val) name
  Core.setInitializer global val
  return global

declareFunction :: Core.Module -> String -> Core.Type -> IO Core.Value
declareFunction mod name typ = do
  maybeFunc <- Core.getNamedFunction mod name
  case maybeFunc of
    Nothing -> Core.addFunction mod name typ
    Just func -> return $ if Core.getElementType (Core.typeOf func) /= typ
                          then Core.constBitCast (Core.pointerType typ) func
                          else func

defineFunction :: Core.Module -> String -> Core.Type
               -> IO (Core.Value, Core.BasicBlock)
defineFunction mod name typ = do
  func <- Core.addFunction mod name typ
  bblk <- Core.appendBasicBlock func "entry"
  return (func, bblk)

buildModule :: IO (Core.Module, Core.Value)
buildModule = do
  mod <- Core.createModule "hello"
  greetz <- defineGlobal mod "greeting" (Core.const "hello jit!")
  puts <- declareFunction mod "puts" (Core.functionType Core.int32Type
                                          [Core.pointerType Core.int8Type])
  (func, entry) <- defineFunction mod "main"
                   (Core.functionType Core.int32Type [])
  bld <- Core.createBuilder
  Core.positionAtEnd bld entry
  let zero = Core.const (0::Int32)
  tmp <- Core.buildGEP bld greetz [zero, zero] "tmp"
  Core.buildCall bld puts [tmp] ""
  Core.buildRet bld zero
  return (mod, func)

execute :: Core.Module -> Core.Value -> IO ()
execute mod func = do
  prov <- Core.createModuleProviderForExistingModule mod
  ee <- EE.createExecutionEngine prov
  EE.runStaticConstructors ee
  EE.runFunction ee func []
  EE.runStaticDestructors ee
  return ()

main :: IO ()
main = buildModule >>= uncurry execute
