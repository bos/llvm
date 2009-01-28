{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, ScopedTypeVariables #-}
module LLVM.Core.Vector(MkVector(..)) where
import Data.Function
import Data.TypeNumbers
import LLVM.Core.Type
import LLVM.Core.Data
import LLVM.Core.CodeGen(IsConst(..), ConstValue(..))
import LLVM.FFI.Core(constVector)
import LLVM.ExecutionEngine.Target
import Foreign.Ptr(Ptr, castPtr)
import Foreign.Storable(Storable(..))
import Foreign.Marshal.Array(peekArray, pokeArray, withArrayLen)
import System.IO.Unsafe(unsafePerformIO)

-- XXX Should these really be here?
class (IsPowerOf2 n, IsPrimitive a) => MkVector va n a | va -> n a, n a -> va where
    toVector :: va -> Vector n a
    fromVector :: Vector n a -> va

{-
instance (IsPrimitive a) => MkVector (Value a) (D1 End) (Value a) where
    toVector a = Vector [a]
-}

instance (IsPrimitive a) => MkVector (a, a) (D2 End) a where
    toVector (a1, a2) = Vector [a1, a2]
    fromVector (Vector [a1, a2]) = (a1, a2)
    fromVector _ = error "fromVector: impossible"

instance (IsPrimitive a) => MkVector (a, a, a, a) (D4 End) a where
    toVector (a1, a2, a3, a4) = Vector [a1, a2, a3, a4]
    fromVector (Vector [a1, a2, a3, a4]) = (a1, a2, a3, a4)
    fromVector _ = error "fromVector: impossible"

instance (IsPrimitive a) => MkVector (a, a, a, a, a, a, a, a) (D8 End) a where
    toVector (a1, a2, a3, a4, a5, a6, a7, a8) = Vector [a1, a2, a3, a4, a5, a6, a7, a8]
    fromVector (Vector [a1, a2, a3, a4, a5, a6, a7, a8]) = (a1, a2, a3, a4, a5, a6, a7, a8)
    fromVector _ = error "fromVector: impossible"

instance (Storable a, IsPowerOf2 n, IsPrimitive a) => Storable (Vector n a) where
    sizeOf a = storeSizeOfType ourTargetData (typeRef a)
    alignment a = aBIAlignmentOfType ourTargetData (typeRef a)
    peek p = fmap Vector $ peekArray (typeNumber (undefined :: n)) (castPtr p :: Ptr a)
    poke p (Vector vs) = pokeArray (castPtr p :: Ptr a) vs

instance (IsPowerOf2 n, IsPrimitive a, IsConst a) => IsConst (Vector n a) where
    constOf (Vector vs) =
        unsafePerformIO $
        withArrayLen [ c | v <- vs, let ConstValue c = constOf v ]  $ \ len ptr ->
        return $ ConstValue $ constVector ptr (fromIntegral len)

--------------------------------------

unVector :: Vector n a -> [a]
unVector (Vector xs) = xs

binop :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
binop op xs ys = Vector $ zipWith op (unVector xs) (unVector ys)

unop :: (a -> b) -> Vector n a -> Vector n b
unop op = Vector . map op . unVector

instance (Eq a) => Eq (Vector n a) where
    (==) = (==) `on` unVector

instance (Ord a) => Ord (Vector n a) where
    compare = compare `on` unVector

instance (Num a, IsTypeNumber n) => Num (Vector n a) where
    (+) = binop (+)
    (-) = binop (-)
    (*) = binop (*)
    negate = unop negate
    abs = unop abs
    signum = unop signum
    fromInteger = Vector . replicate (typeNumber (undefined :: n)) . fromInteger

instance (Enum a, IsTypeNumber n) => Enum (Vector n a) where
    succ = unop succ
    pred = unop pred
    fromEnum = error "Vector fromEnum"
    toEnum = Vector . map toEnum . replicate (typeNumber (undefined :: n))

instance (Real a, IsTypeNumber n) => Real (Vector n a) where
    toRational = error "Vector toRational"

instance (Integral a, IsTypeNumber n) => Integral (Vector n a) where
    quot = binop quot
    rem  = binop rem
    div  = binop div
    mod  = binop mod
    quotRem (Vector xs) (Vector ys) = (Vector qs, Vector rs) where (qs, rs) = unzip $ zipWith quotRem xs ys
    divMod  (Vector xs) (Vector ys) = (Vector qs, Vector rs) where (qs, rs) = unzip $ zipWith divMod  xs ys
    toInteger = error "Vector toInteger"

instance (Fractional a, IsTypeNumber n) => Fractional (Vector n a) where
    (/) = binop (/)
    fromRational = Vector . replicate (typeNumber (undefined :: n)) . fromRational

instance (RealFrac a, IsTypeNumber n) => RealFrac (Vector n a) where
    properFraction = error "Vector properFraction"

instance (Floating a, IsTypeNumber n) => Floating (Vector n a) where
    pi = Vector $ replicate (typeNumber (undefined :: n)) pi
    sqrt = unop sqrt
    log = unop log
    logBase = binop logBase
    (**) = binop (**)
    exp = unop exp
    sin = unop sin
    cos = unop cos
    tan = unop tan
    asin = unop asin
    acos = unop acos
    atan = unop atan
    sinh = unop sinh
    cosh = unop cosh
    tanh = unop tanh
    asinh = unop asinh
    acosh = unop acosh
    atanh = unop atanh

instance (RealFloat a, IsTypeNumber n) => RealFloat (Vector n a) where
    floatRadix = floatRadix . head . unVector
    floatDigits = floatDigits . head . unVector
    floatRange = floatRange . head . unVector
    decodeFloat = error "Vector decodeFloat"
    encodeFloat = error "Vector encodeFloat"
    exponent _ = 0
    scaleFloat 0 x = x
    scaleFloat _ _ = error "Vector scaleFloat"
    isNaN = error "Vector isNaN"
    isInfinite = error "Vector isInfinite"
    isDenormalized = error "Vector isDenormalized"
    isNegativeZero = error "Vector isNegativeZero"
    isIEEE = isIEEE . head . unVector
