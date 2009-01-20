{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}
module DotProd where
import Data.Word
import Data.TypeNumbers
import LLVM.Core
import LLVM.ExecutionEngine
import LLVM.Util.Loop
import LLVM.Util.Foreign

mDotProd :: forall n a . (IsPowerOf2 n, IsTypeNumber n,
	                  IsPrimitive a, IsArithmetic a, IsFirstClass a, IsConst a, Num a,
	                  FunctionRet a
	                 ) =>
            CodeGenModule (Function (Word32 -> Ptr (Vector n a) -> Ptr (Vector n a) -> IO a))
mDotProd =
  createFunction ExternalLinkage $ \ size aPtr bPtr -> do
    s <- forLoop (valueOf 0) size (value (zero :: ConstValue (Vector n a))) $ \ i s -> do

        ap <- getElementPtr aPtr (i, ()) -- index into aPtr
        bp <- getElementPtr bPtr (i, ()) -- index into bPtr
        a <- load ap                     -- load element from a vector
        b <- load bp                     -- load element from b vector
        ab <- mul a b                    -- multiply them
        add s ab                         -- accumulate sum

    r <- forLoop (valueOf (0::Word32)) (valueOf (typeNumber (undefined :: n)))
                 (valueOf 0) $ \ i r -> do
        ri <- extractelement s i
        add r ri
    ret (r :: Value a)

type R = Float
type T = Vector (D4 End) R

main :: IO ()
main = do
    let mDotProd' = mDotProd
    writeFunction "DotProd.bc" mDotProd'

    ioDotProd <- simpleFunction mDotProd'
    let dotProd :: [T] -> [T] -> R
        dotProd a b =
         unsafePurify $
         withArrayLen a $ \ aLen aPtr ->
         withArrayLen b $ \ bLen bPtr ->
         ioDotProd (fromIntegral (aLen `min` bLen)) aPtr bPtr


    let a = [1 .. 8]
        b = [4 .. 11]
    print $ dotProd (vectorize 0 a) (vectorize 0 b)
    print $ sum $ zipWith (*) a b

writeFunction :: String -> CodeGenModule a -> IO ()
writeFunction name f = do
    m <- newModule
    defineModule m f
    writeBitcodeToFile name m

class Vectorize n a where
    vectorize :: a -> [a] -> [Vector n a]

{-
instance (IsPrimitive a) => Vectorize (D1 End) a where
    vectorize _ [] = []
    vectorize x (x1:xs) = toVector x1 : vectorize x xs
-}

instance (IsPrimitive a) => Vectorize (D2 End) a where
    vectorize _ [] = []
    vectorize x (x1:x2:xs) = toVector (x1, x2) : vectorize x xs
    vectorize x xs = vectorize x $ xs ++ [x]

instance (IsPrimitive a) => Vectorize (D4 End) a where
    vectorize _ [] = []
    vectorize x (x1:x2:x3:x4:xs) = toVector (x1, x2, x3, x4) : vectorize x xs
    vectorize x xs = vectorize x $ xs ++ [x]

instance (IsPrimitive a) => Vectorize (D8 End) a where
    vectorize _ [] = []
    vectorize x (x1:x2:x3:x4:x5:x6:x7:x8:xs) = toVector (x1, x2, x3, x4, x5, x6, x7, x8) : vectorize x xs
    vectorize x xs = vectorize x $ xs ++ [x]
