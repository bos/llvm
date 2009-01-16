{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Arith where
import Data.Int
import Data.TypeNumbers
import LLVM.Core
import LLVM.ExecutionEngine
import LLVM.Util.Arithmetic

mSomeFn :: forall a . (IsConst a, Floating a, IsFloating a, Cmp a,
	      FunctionRet a
	     ) => CodeGenModule (Function (a -> IO a))
mSomeFn = do
    foo <- createFunction InternalLinkage $ arithFunction $ \ x y -> exp (sin x) + y
    let foo' = toArithFunction foo
    createFunction ExternalLinkage $ arithFunction $ \ x -> do
        y <- set $ x^3
        sqrt (x^2 - 5 * x + 6) + foo' x x + y + log y

mFib :: CodeGenModule (Function (Int32 -> IO Int32))
mFib = recursiveFunction $ \ rfib n -> n %< 2 ? (1, rfib (n-1) + rfib (n-2))

type V = Vector (D4 End) Float

mVFun :: CodeGenModule (Function (V -> IO V))
mVFun = do
{-
    vlogf' <- newNamedFunction ExternalLinkage "vlogf"
    let vlogf :: TValue r V -> TValue r V
        vlogf x = toArithFunction vlogf' x
--        vlogf x = do x' <- x; call vlogf' x'
    vexpf' <- newNamedFunction ExternalLinkage "vexpf"
    let vexpf :: TValue r V -> TValue r V
--	vexpf x = do x' <- x; call vexpf' x'
        vexpf x = toArithFunction vexpf' x
-}
    createFunction ExternalLinkage $ arithFunction $ \ x ->
        vlogf x * vexpf x * x - 16


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

    writeFunction "VArith.bc" mVFun

    writeFunction "Arith.bc" mSomeFn'

    print (someFn 10)
    print (someFn 2)

    writeFunction "ArithFib.bc" mFib

    fib <- simpleFunction mFib
    fib 22 >>= print
