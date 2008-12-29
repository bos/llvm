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
    -- A global variable that vectest messes with.
    acc <- createNamedGlobal False ExternalLinkage "acc" (constOf (0 :: T))

    -- Return the global variable.
    retAcc <- createNamedFunction ExternalLinkage "retacc" $ do
        vacc <- load acc
        ret vacc
    let _ = retAcc :: Function (IO T)  -- Force the type of retAcc.

    -- A function that tests vector opreations.
    f <- createNamedFunction ExternalLinkage "vectest" $ \ x -> do

        let v = value (zero :: ConstValue (Vector N T))
	    n = typeNumber (undefined :: N) :: Word32

        -- Fill the vector with x, x+1, x+2, ...
        (_, v1) <- forLoop (valueOf 0) (valueOf n) (x, v) $ \ i (x1, v1) -> do
            x1' <- add x1 (1::T)
	    v1' <- insertelement v1 x1 i
	    return (x1', v1')

	-- Elementwise cubing of the vector.
	vsq <- mul v1 v1
        vcb <- mul vsq v1

        -- Sum the elements of the vector.
        s <- forLoop (valueOf 0) (valueOf n) (valueOf 0) $ \ i s -> do
            y <- extractelement vcb i
     	    s' <- add s (y :: Value T)
	    return s'

        -- Update the global variable.
        vacc <- load acc
        vacc' <- add vacc s
        store vacc' acc

        ret (s :: Value T)

--    liftIO $ dumpValue f
    return f

-- Run LLVM optimizer at standard level.
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
    -- XXX Using createExecutionEngine more than once aborts the LLVM.
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
	ioretacc' :: Function (IO T)
        Just ioretacc' = castModuleValue $ fromJust $ lookup "retacc" funcs
    ee' <- createModuleProviderForExistingModule m' >>= createExecutionEngine
    let vec' = generateFunction ee' iovec'
        retacc' = generateFunction ee' ioretacc'

    dumpValue iovec'

    x <- vec' 10
    print x
    y <- vec' 0
    print y
    z <- retacc'
    print z
