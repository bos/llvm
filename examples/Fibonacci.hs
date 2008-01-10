module Fibonacci where
import Prelude hiding (and, or)
import System.Environment (getArgs)
import Control.Monad(forM_)
import Data.Word

import LLVM.Core
import LLVM.ExecutionEngine

main :: IO ()
main = do
  args <- getArgs
  let args' = if null args then ["10"] else args

  m <- newModule "fib"
  fns <- defineModule m buildMod
  dumpValue $ mfib fns
  dumpValue $ mplus fns

  prov <- createModuleProviderForExistingModule m
  ee <- createExecutionEngine prov
  
  let fib = unsafeGeneratePureFunction ee (mfib fns)

  forM_ args' $ \num -> do
      putStrLn $ "fib " ++ num ++ " = " ++ show (fib (read num))
  return ()

data Mod = Mod {
    mfib :: Function (Word32 -> Word32),
    mplus :: Function (Word32 -> Word32 -> Word32)
    }

buildMod :: CodeGenModule Mod
buildMod = do
  -- Add two numbers in a cumbersome way.
  plus <- createFunction $ \ x y -> do
    l1 <- newBasicBlock
    l2 <- newBasicBlock
    l3 <- newBasicBlock

    a <- and x (1 :: Word32)
    c <- icmp IntEQ a (0 :: Word32)
    condBr c l1 l2

    defineBasicBlock l1
    r1 <- add x y
    br l3

    defineBasicBlock l2
    r2 <- add y x
    br l3
    defineBasicBlock l3

    r <- phi [(r1, l1), (r2, l2)]
    ret r

  fib <- newFunction
  defineFunction fib $ \ arg -> do
    recurse <- newBasicBlock
    exit <- newBasicBlock

    test <- icmp IntUGT arg (2::Word32)
    condBr test recurse exit

    defineBasicBlock exit
    ret (1::Word32)

    defineBasicBlock recurse
    x1 <- sub arg (1::Word32)
    fibx1 <- call fib x1
    x2 <- sub arg (2::Word32)
    fibx2 <- call fib x2
    r <- call plus fibx1 fibx2
    ret r

  return $ Mod fib plus
