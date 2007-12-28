module HelloJit (main) where

import Data.Int (Int32)
import qualified LLVM
import qualified LLVM.ExecutionEngine as EE

defineGlobal :: LLVM.Module -> String -> LLVM.Value -> IO LLVM.Value
defineGlobal mod name val = do
  global <- LLVM.addGlobal mod (LLVM.typeOf val) name
  LLVM.setInitializer global val
  return global

declareFunction :: LLVM.Module -> String -> LLVM.Type -> IO LLVM.Value
declareFunction mod name typ = do
  maybeFunc <- LLVM.getNamedFunction mod name
  case maybeFunc of
    Nothing -> LLVM.addFunction mod name typ
    Just func -> return $ if LLVM.getElementType (LLVM.typeOf func) /= typ
                          then LLVM.constBitCast (LLVM.pointerType typ) func
                          else func

defineFunction :: LLVM.Module -> String -> LLVM.Type
               -> IO (LLVM.Value, LLVM.BasicBlock)
defineFunction mod name typ = do
  func <- LLVM.addFunction mod name typ
  bblk <- LLVM.appendBasicBlock func "entry"
  return (func, bblk)

buildModule :: IO (LLVM.Module, LLVM.Value)
buildModule = do
  mod <- LLVM.createModule "hello"
  greetz <- defineGlobal mod "greeting" (LLVM.const "hello jit!")
  puts <- declareFunction mod "puts" (LLVM.functionType LLVM.int32Type
                                          [LLVM.pointerType LLVM.int8Type])
  (main, entry) <- defineFunction mod "main"
                   (LLVM.functionType LLVM.int32Type [])
  bld <- LLVM.createBuilder
  LLVM.positionAtEnd bld entry
  let zero = LLVM.const (0::Int32)
  tmp <- LLVM.buildGEP bld greetz [zero, zero] "tmp"
  LLVM.buildCall bld puts [tmp] ""
  LLVM.buildRet bld zero
  return (mod, main)

execute :: LLVM.Module -> LLVM.Value -> IO ()
execute mod func = do
  prov <- LLVM.createModuleProviderForExistingModule mod
  ee <- EE.createExecutionEngine prov
  {-
  EE.runStaticConstructors ee
  EE.runFunction ee func []
  EE.runStaticDestructors ee
  -}
  return ()

main :: IO ()
main = do
  buildModule >>= uncurry execute
  print "woo"
