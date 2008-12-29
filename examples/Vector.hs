module Vector where
import System.Process(system)
--import Control.Monad.Trans(liftIO)
import Data.Maybe(fromJust)
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
    _v <- createNamedGlobal False ExternalLinkage "var" (constOf (42 :: Float))
    f <- createNamedFunction ExternalLinkage "vectest" $ \ x -> do

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

optimize :: String -> IO ()
optimize name = do
    _rc <- system $ "opt -std-compile-opts " ++ name ++ " -f -o " ++ name
    return ()

main :: IO ()
main = do
    let name = "Vec.bc"
    m <- newModule
    _iovec <- defineModule m cgvec
    writeBitcodeToFile name m

{-
    ee <- createModuleProviderForExistingModule m >>= createExecutionEngine
    let vec = unsafePurify $ generateFunction ee $ iovec

    print $ vec 10
-}
    optimize name

    m' <- readBitcodeFromFile name
    funcs <- getModuleValues m'
    print $ map fst funcs

    let iovec' :: Function (T -> IO T)
        Just iovec' = castModuleValue $ fromJust $ lookup "vectest" funcs
    ee' <- createModuleProviderForExistingModule m' >>= createExecutionEngine
    let vec' = unsafePurify $ generateFunction ee' $ iovec'

    dumpValue iovec'
    print $ vec' 10

