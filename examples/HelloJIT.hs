{-# LANGUAGE TypeOperators #-}
module HelloJIT (main) where

import Data.Int (Int32)
import Prelude hiding (mod)

import LLVM.Core.Type ((:->)(..))
import qualified LLVM.Core as Core
import qualified LLVM.Core.Builder as B
import qualified LLVM.Core.Constant as C
import qualified LLVM.Core.Type as T
import qualified LLVM.Core.Value as V
import qualified LLVM.Core.Utils as U
import qualified LLVM.ExecutionEngine as EE


buildModule :: IO (T.Module, V.Function T.Int32)
buildModule = do
  mod <- Core.createModule "hello"
  greetz <- U.defineGlobal mod "greeting" (C.const "hello jit!")
  let t = undefined :: T.Pointer T.Int8 :-> T.Int32
  putStrLn $ "type of puts: " ++ show t
  puts <- U.declareFunction mod "puts" (T.function t)
  print (T.function t)
  (func, entry) <- U.defineFunction mod "main"
                   (T.function (undefined :: T.Int32))
  bld <- B.createBuilder
  B.positionAtEnd bld entry
  let zero = C.const (0::Int32)
  tmp <- B.getArrayPtr bld "tmp" greetz [zero, zero]
  B.call_ bld "" puts (tmp :-> zero)
  B.ret bld zero
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
