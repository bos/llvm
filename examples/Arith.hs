{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Arith where
import Data.Int
import Data.TypeLevel(D4)
import LLVM.Core
import LLVM.ExecutionEngine
import LLVM.Util.Arithmetic
import LLVM.Util.Foreign as F
import LLVM.Util.File(writeCodeGenModule)

import Foreign.Storable
{-
import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Marshal.Alloc as F
-}

mSomeFn :: forall a b . (IsConst a, Floating a, IsFloating a, CallIntrinsic a,
	                 FunctionRet a, Cmp a b
                        ) => CodeGenModule (Function (a -> IO a))
mSomeFn = do
    foo <- createFunction InternalLinkage $ arithFunction $ \ x y -> exp (sin x) + y
    let foo' = toArithFunction foo
    createFunction ExternalLinkage $ arithFunction $ \ x -> do
        y <- set $ x^3
        sqrt (x^2 - 5 * x + 6) + foo' x x + y + log y

mFib :: CodeGenModule (Function (Int32 -> IO Int32))
mFib = recursiveFunction $ \ rfib n -> n %< 2 ? (1, rfib (n-1) + rfib (n-2))

type V = Vector D4 Float

mVFun :: CodeGenModule (Function (Ptr V -> Ptr V -> IO ()))
mVFun = do
    fn :: Function (V -> IO V)
       <- createFunction ExternalLinkage $ arithFunction $ \ x ->
            log x * exp x * x - 16

    vectorToPtr fn


main :: IO ()
main = do
    -- Initialize jitter
    initializeNativeTarget

    let mSomeFn' = mSomeFn
    ioSomeFn <- simpleFunction mSomeFn'
    let someFn :: Double -> Double
        someFn = unsafePurify ioSomeFn

    writeCodeGenModule "Arith.bc" mSomeFn'

    print (someFn 10)
    print (someFn 2)

    writeCodeGenModule "ArithFib.bc" mFib

    fib <- simpleFunction mFib
    fib 22 >>= print

{-
    writeCodeGenModule "VArith.bc" mVFun

    ioVFun <- simpleFunction mVFun
    let v = toVector (1,2,3,4)

    r <- vectorPtrWrap ioVFun v
    print r
-}

vectorToPtr :: Function (V -> IO V) -> CodeGenModule (Function (Ptr V -> Ptr V -> IO ()))
vectorToPtr f =
    createFunction ExternalLinkage $ \ px py -> do
        x <- load px
        y <- call f x
        store y py
	ret ()

vectorPtrWrap :: (Ptr V -> Ptr V -> IO ()) -> V -> IO V
vectorPtrWrap f v =
    with v $ \ aPtr ->
        F.alloca $ \ bPtr -> do
             f aPtr bPtr
             peek bPtr
