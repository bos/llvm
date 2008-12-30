module Array where
import Data.Word

import LLVM.Core
--import LLVM.ExecutionEngine

import Loop

cg = do
    dotProd <- createFunction InternalLinkage $ \ size aPtr aStride bPtr bStride -> do
        r <- forLoop (valueOf 0) size (valueOf 0) $ \ i s -> do
	    ai <- mul aStride i
	    bi <- mul bStride i
            ap <- getElementPtr aPtr (ai, ())
            bp <- getElementPtr bPtr (bi, ())
            a <- load ap
            b <- load bp
            ab <- mul a b
            add (s :: Value Double) ab
	ret r
    let _ = dotProd :: Function (Word32 -> Ptr Double -> Word32 -> Ptr Double -> Word32 -> IO Double)

    -- multiply a:[n x m], b:[m x l]
    matMul <- createFunction ExternalLinkage $ \ n m l aPtr bPtr -> do
        size <- mul n l
        c <- arrayMalloc size
        forLoop (valueOf 0) n () $ \ ni () -> do
           forLoop (valueOf 0) l () $ \ li () -> do
	      ni' <- mul ni m
	      row <- getElementPtr aPtr (ni', ())
	      col <- getElementPtr bPtr (li, ())
              x <- call dotProd m row (valueOf 1) col m
	      j <- add ni' li
	      p <- getElementPtr c (j, ())
	      store x p
	      return ()
        ret c
    let _ = matMul :: Function (Word32 -> Word32 -> Word32 -> Ptr Double -> Ptr Double -> IO (Ptr Double))

    return matMul

main :: IO ()
main = do
    m <- newModule
    _f <- defineModule m cg
    writeBitcodeToFile "Arr.bc" m
