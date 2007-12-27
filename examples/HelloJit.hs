module HelloJit (main) where

import qualified LLVM

defineGlobal :: LLVM.Module -> String -> LLVM.Value -> IO LLVM.Value
defineGlobal mod name val = do
  global <- LLVM.addGlobal mod (LLVM.typeOf val) name
  LLVM.setInitializer global val
  return global

declareFunction :: LLVM.Module -> LLVM.Type -> String -> IO LLVM.Value
declareFunction mod typ name = do
  maybeFunc <- LLVM.getNamedFunction name
  case maybeFunc of
    Nothing -> LLVM.addFunction mod typ name
    Just func -> return $ if LLVM.getElementType (LLVM.typeOf func) /= typ
                          then LLVM.constBitCast (LLVM.pointerType typ 0) func
                          else func

main :: IO ()
main = do
  mod <- LLVM.createModule "hello"
  greetz <- defineGlobal mod "greeting" (LLVM.const "hello jit!")
  -- puts <- declareFunction mod "puts"
  print "woo"
