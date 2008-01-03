{-# LANGUAGE TypeOperators #-}

module HowToUseJIT (main) where

import LLVM.Core.Type ((:->)(..))
import qualified LLVM.Core as Core
import qualified LLVM.Core.Builder as B
import qualified LLVM.Core.Constant as C
import qualified LLVM.Core.Type as T
import qualified LLVM.Core.Value as V
import qualified LLVM.Core.Utils as U
import Data.Int (Int32)

main :: IO ()
main = do
  m <- Core.createModule "test"
  let t = undefined :: T.Int32 :-> T.Int32 :-> T.Int32
  (add1, addEntry) <- U.defineFunction m "add1" (T.function t)
  let a :-> b :-> _ = V.params add1
  -- mapM_ V.dumpValue [a,b,ret]
  bld <- B.createBuilder
  B.positionAtEnd bld addEntry
  v1 <- B.add bld "" (C.const (1::Int32)) a
  v2 <- B.add bld "" v1 b
  B.ret bld v2
  (foo, fooEntry) <- U.defineFunction m "foo" (T.function (undefined :: T.Int32))
  B.positionAtEnd bld fooEntry
  c <- B.call bld "" add1 [V.anyValue (C.const (1::Int32)), V.anyValue (C.const (10::Int32))]
  B.ret bld (c::V.Instruction T.Int32)
  V.dumpValue foo
  return ()
