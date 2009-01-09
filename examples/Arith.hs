{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, FlexibleContexts, UndecidableInstances #-}
module Arith where
--import Control.Monad
import Data.Int
import LLVM.Core
import LLVM.ExecutionEngine

instance (Show (CodeGenFunction r (Value a)))
instance (Eq (CodeGenFunction r (Value a)))
instance (Ord (CodeGenFunction r (Value a)))

binop :: (Value a -> Value a -> CodeGenFunction r (Value a)) ->
         CodeGenFunction r (Value a) -> CodeGenFunction r (Value a) -> CodeGenFunction r (Value a)
binop op x y = do
    x' <- x
    y' <- y
    op x' y'

instance (Num a, IsArithmetic a, IsConst a) => Num (CodeGenFunction r (Value a)) where
    (+) = binop add
    (-) = binop sub
    (*) = binop mul
    negate = (>>= neg)
    abs _x = error "XXX"
    signum _x = error "XXX"
    fromInteger = return . valueOf . fromInteger

instance (Num a, IsConst a, IsArithmetic a) => Enum (CodeGenFunction r (Value a)) where
    succ x = x + 1
    pred x = x - 1
    fromEnum _ = error "CodeGenFunction Value: fromEnum"
    toEnum = fromIntegral

instance (Num a, IsConst a, IsArithmetic a) => Real (CodeGenFunction r (Value a)) where
    toRational _ = error "CodeGenFunction Value: toRational"

instance (Num a, IsConst a, IsInteger a) => Integral (CodeGenFunction r (Value a)) where
    quot = binop (if (isSigned (undefined :: a)) then sdiv else udiv)
    rem  = binop (if (isSigned (undefined :: a)) then srem else urem)
    quotRem x y = (quot x y, rem x y)
    toInteger _ = error "CodeGenFunction Value: toInteger"

instance (Fractional a, IsConst a, IsFloating a) => Fractional (CodeGenFunction r (Value a)) where
    (/) = binop fdiv
    fromRational = return . valueOf . fromRational

instance (Fractional a, IsConst a, IsFloating a) => RealFrac (CodeGenFunction r (Value a)) where
    properFraction _ = error "CodeGenFunction Value: properFraction"

instance (Floating a, IsConst a, IsFloating a) => Floating (CodeGenFunction r (Value a)) where
    pi = return $ valueOf pi

instance (RealFloat a, IsConst a, IsFloating a) => RealFloat (CodeGenFunction r (Value a)) where
    floatRadix _ = floatRadix (undefined :: a)
    floatDigits _ = floatDigits (undefined :: a)
    floatRange _ = floatRange (undefined :: a)
    decodeFloat _ = error "CodeGenFunction Value: decodeFloat"
    encodeFloat _ _ = error "CodeGenFunction Value: encodeFloat"
    exponent _ = 0
    scaleFloat 0 x = x
    scaleFloat _ _ = error "CodeGenFunction Value: scaleFloat"
    isNaN _ = error "CodeGenFunction Value: isNaN"
    isInfinite _ = error "CodeGenFunction Value: isInfinite"
    isDenormalized _ = error "CodeGenFunction Value: isDenormalized"
    isNegativeZero _ = error "CodeGenFunction Value: isNegativeZero"
    isIEEE _ = isIEEE (undefined :: a)

class (FunctionArgs (IO a) (CodeGenFunction a ()) (CodeGenFunction a ())) => FunctionRet a
instance (FunctionArgs (IO a) (CodeGenFunction a ()) (CodeGenFunction a ())) => FunctionRet a

mSomePoly :: (IsFirstClass a, IsConst a, Num a,
	      FunctionRet a
	     ) => CodeGenModule (Function (a -> IO a))
mSomePoly =
    createFunction ExternalLinkage $ \ ax -> do
        let x = return ax
        r <- x^2 - 5 * x + 6
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
    let somePoly :: Int64 -> Int64
        somePoly = unsafePurify ioSomePoly

    writeFunction "Arith.bc" mSomePoly'

    print (somePoly 10)
    print (somePoly 2)


