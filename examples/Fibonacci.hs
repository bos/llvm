{-# LANGUAGE TypeOperators #-}

module Fibonacci (main) where

import Control.Monad (forM_)
import Data.Int (Int32)
import System.Environment (getArgs)

import qualified LLVM.Core as Core
import qualified LLVM.Core.Builder as B
import qualified LLVM.Core.Constant as C
import qualified LLVM.Core.Instruction as I
import qualified LLVM.Core.Type as T
import qualified LLVM.Core.Value as V
import qualified LLVM.Core.Utils as U
import qualified LLVM.ExecutionEngine as EE

buildFib :: T.Module -> IO (V.Function T.Int32 T.Int32)
buildFib m = do
  let one = C.const (1::Int32)
      two = C.const (2::Int32)
  (fib, entry) <- U.defineFunction m "fib" (T.function undefined undefined)
  bld <- B.createBuilder
  exit <- Core.appendBasicBlock fib "return"
  recurse <- Core.appendBasicBlock fib "recurse"
  let arg = V.params fib

  B.positionAtEnd bld entry
  test <- B.icmp bld "" I.IntSLE arg two
  B.condBr bld test exit recurse

  B.positionAtEnd bld exit
  B.ret bld one

  B.positionAtEnd bld recurse
  x1 <- B.sub bld "" arg one
  fibx1 <- B.call bld "" fib x1

  x2 <- B.sub bld "" arg two
  fibx2 <- B.call bld "" fib x2

  B.add bld "" fibx1 fibx2 >>= B.ret bld
  return fib

main :: IO ()
main = do
  args <- getArgs
  let args' = if null args then ["10"] else args

  m <- Core.createModule "fib"
  fib <- buildFib m
  V.dumpValue fib

  prov <- Core.createModuleProviderForExistingModule m
  ee <- EE.createExecutionEngine prov
  
  forM_ args' $ \num -> do
    putStr $ "fib " ++ num ++ " = "
    parm <- EE.createGeneric (read num :: Int)
    gv <- EE.runFunction ee fib [parm]
    print (EE.fromGeneric gv :: Int)
