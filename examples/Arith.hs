{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Arith where
--import Data.Int
import LLVM.Core
import LLVM.ExecutionEngine
import LLVM.Util.Arithmetic

mSomeFn :: forall a . (IsFirstClass a, IsConst a, Floating a, IsFloating a, Cmp a,
	      FunctionRet a
	     ) => CodeGenModule (Function (a -> IO a))
mSomeFn = do
    foo <- createFunction InternalLinkage $ arithFunction $ exp . sin
    let _ = foo :: Function (a -> IO a)
        foo' x = x >>= call foo
    createFunction ExternalLinkage $ arithFunction $ \ x ->
        sqrt (x^2 - 5 * x + 6) + foo' x

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


