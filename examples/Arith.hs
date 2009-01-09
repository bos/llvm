{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Arith where
import Data.Int
import LLVM.Core
import LLVM.ExecutionEngine
import LLVM.Util.Arithmetic

mSomeFn :: forall a . (IsFirstClass a, IsConst a, Floating a, IsFloating a, Cmp a,
	      FunctionRet a
	     ) => CodeGenModule (Function (a -> IO a))
mSomeFn = do
    foo <- createFunction InternalLinkage $ arithFunction $ \ x y -> exp (sin x) + y
    createFunction ExternalLinkage $ arithFunction $ \ x ->
        sqrt (x^2 - 5 * x + 6) + toArithFunction foo x x

mFib :: CodeGenModule (Function (Int32 -> IO Int32))
mFib = recursiveFunction $ \ rfib n -> n %< 2 ? (1, rfib (n-1) + rfib (n-2))

writeFunction :: String -> CodeGenModule a -> IO ()
writeFunction name f = do
    m <- newModule
    defineModule m f
    writeBitcodeToFile name m

main :: IO ()
main = do
    let mSomeFn' = mSomeFn
    ioSomeFn <- simpleFunction mSomeFn'
    let someFn :: Double -> Double
        someFn = unsafePurify ioSomeFn

    writeFunction "Arith.bc" mSomeFn'

    print (someFn 10)
    print (someFn 2)

    writeFunction "ArithFib.bc" mFib

    fib <- simpleFunction mFib
    fib 22 >>= print

