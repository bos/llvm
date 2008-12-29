module Vector where
import Control.Monad.Trans(liftIO)
import Data.TypeNumbers
import Data.Word

import LLVM.Core
import LLVM.ExecutionEngine

import Loop

-- Type of vector elements.
type T = Float

-- Number of vector elements.
type N = D1 (D6 End)

cgvec :: CodeGenModule (Function (T -> IO T))
cgvec = do
    f <- createFunction ExternalLinkage $ \ x -> do

        let v = value (zero :: ConstValue (Vector N T))
	    n = typeNumber (undefined :: N) :: Word32

        (_, v1) <- forLoop (valueOf 0) (valueOf n) (x, v) $ \ i (x1, v1) -> do
            x1' <- add x1 (1::T)
	    v1' <- insertelement v1 x1 i
	    return (x1', v1')

	-- Elementwise cubing of the vector.
	vsq <- mul v1 v1
        vcb <- mul vsq v1

        (OneTuple s) <- forLoop (valueOf 0) (valueOf n) (OneTuple (valueOf 0)) $ \ i (OneTuple s) -> do
            y <- extractelement vcb i
     	    s' <- add s (y :: Value T)
	    return (OneTuple s')

        ret (s :: Value T)

--    liftIO $ dumpValue f
    return f

main :: IO ()
main = do
    m <- newModule
    iovec <- defineModule m cgvec
    writeBitcodeToFile "Vec.bc" m

{-
    ee <- createModuleProviderForExistingModule m >>= createExecutionEngine
    let vec = unsafePurify $ generateFunction ee $ iovec

    print $ vec 10
-}
    m' <- readBitcodeFromFile "Vec.bc"
    [(name, func)] <- getModuleFunctions m'
    let iovec' :: Function (T -> IO T)
        Just iovec' = castModuleFunction func
    ee' <- createModuleProviderForExistingModule m' >>= createExecutionEngine
    let vec' = unsafePurify $ generateFunction ee' $ iovec'

    print name
    print $ vec' 10
