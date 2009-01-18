{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Arith where
import Data.Int
import Data.TypeNumbers
import LLVM.Core
import LLVM.ExecutionEngine
import LLVM.Util.Arithmetic
import LLVM.Util.Foreign as F

import Foreign.Storable
{-
import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc as F
-}

mSomeFn :: forall a . (IsConst a, Floating a, IsFloating a, CallIntrinsic a,
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

--mVFun :: CodeGenModule (Function (V -> IO V))
mVFun :: CodeGenModule (Function (Ptr V -> Ptr V -> IO ()))
mVFun = do
    fn :: Function (V -> IO V)
       <- createFunction ExternalLinkage $ arithFunction $ \ x ->
            log x * exp x * x - 16

    createFunction ExternalLinkage $ \ px py -> do
        x <- load px
        y <- call fn x
        store y py
	ret ()


writeFunction :: String -> CodeGenModule a -> IO ()
writeFunction name f = do
    m <- newModule
    defineModule m f
    writeBitcodeToFile name m


main :: IO ()
main = do
{-
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
-}

    writeFunction "VArith.bc" mVFun

    ioVFun <- simpleFunction mVFun
    let --vfun = unsafePurify ioVFun
        v :: V
        v = mkVector (1,2,3,4)

--    print $ vfun v
    r <- with v $ \ aPtr ->
           F.alloca $ \ bPtr -> do
             ioVFun aPtr bPtr
             peek bPtr
    print r

    return ()
