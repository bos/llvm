module Arith where
--import Data.Int
import LLVM.Core
import LLVM.ExecutionEngine
import LLVM.Util.Arithmetic

mSomePoly :: (IsFirstClass a, IsConst a, Floating a, IsFloating a, Cmp a,
	      FunctionRet a
	     ) => CodeGenModule (Function (a -> IO a))
mSomePoly =
    createFunction ExternalLinkage $ \ ax -> do
        let x = return ax
        r <- sqrt (x^2 - 5 * x + 6) + sqrt x
        ret r

writeFunction :: String -> CodeGenModule a -> IO ()
writeFunction name f = do
    m <- newModule
    defineModule m f
    writeBitcodeToFile name m

main :: IO ()
main = do
    let mSomePoly' = mSomePoly
    ioSomePoly <- simpleFunction mSomePoly'
    let somePoly :: Double -> Double
        somePoly = unsafePurify ioSomePoly

    writeFunction "Arith.bc" mSomePoly'

    print (somePoly 10)
    print (somePoly 2)


