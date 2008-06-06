{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, FlexibleInstances, IncoherentInstances #-}
-- |The LLVM type system is captured with a number of Haskell type classes.
-- In general, an LLVM type @T@ is represented as @Value T@, where @T@ is some Haskell type.
-- The various types @T@ are classified by various type classes, e.g., 'IsFirstClass' for
-- those types that are LLVM first class types (passable as arguments etc).
-- All valid LLVM types belong to the 'IsType' class.
module LLVM.Core.Type(
    -- * Type classifier
    IsType(..),
    -- ** Special type classifiers
    IsArithmetic,
    IsInteger,
    IsFloating,
    IsPrimitive,
    IsFirstClass,
    IsSized,
    IsFunction,
--    IsFunctionRet,
    IsSequence
    ) where
import Data.Int
import Data.Word
import Data.TypeNumbers
import LLVM.Core.Util(functionType)
import LLVM.Core.Data
import qualified LLVM.Core.FFI as FFI

-- TODO:
-- Move IntN, WordN to a special module that implements those types
--   properly in Haskell.
-- Also more Array and Vector to a Haskell module to implement them.
-- Add Label?
-- Add structures (using tuples, maybe nested).

-- |The 'IsType' class classifies all types that have an LLVM representation.
class IsType a where
    typeRef :: a -> FFI.TypeRef  -- ^The argument is never evaluated

-- |Arithmetic types, i.e., integral and floating types.
class IsType a => IsArithmetic a

-- |Integral types.
class IsArithmetic a => IsInteger a

-- |Floating types.
class IsArithmetic a => IsFloating a

-- |Primitive types.
class IsType a => IsPrimitive a

-- |First class types, i.e., the types that can be passed as arguments, etc.
class IsType a => IsFirstClass a

-- XXX use kind annotation
-- |Sequence types, i.e., vectors and arrays
class IsSequence c where dummy__ :: c a -> a; dummy__ = undefined

-- |Types with a fixed size.
class (IsType a) => IsSized a

-- |Function type.
class (IsType a) => IsFunction a where
    funcType :: [FFI.TypeRef] -> a -> FFI.TypeRef

-- Only make instances for types that make sense in Haskell
-- (i.e., some floating types are excluded).

-- Floating point types.
instance IsType Float  where typeRef _ = FFI.floatType
instance IsType Double where typeRef _ = FFI.doubleType
instance IsType FP128  where typeRef _ = FFI.fp128Type

-- Void type
instance IsType ()     where typeRef _ = FFI.voidType

-- Label type
--data Label
--instance IsType Label  where typeRef _ = FFI.labelType

-- Variable size integer types
instance (IsTypeNumber n) => IsType (IntN n)
    where typeRef _ = FFI.integerType (typeNumber (undefined :: n))

instance (IsTypeNumber n) => IsType (WordN n)
    where typeRef _ = FFI.integerType (typeNumber (undefined :: n))

-- Fixed size integer types.
instance IsType Bool   where typeRef _ = FFI.int1Type
instance IsType Word8  where typeRef _ = FFI.int8Type
instance IsType Word16 where typeRef _ = FFI.int16Type
instance IsType Word32 where typeRef _ = FFI.int32Type
instance IsType Word64 where typeRef _ = FFI.int64Type
instance IsType Int8   where typeRef _ = FFI.int8Type
instance IsType Int16  where typeRef _ = FFI.int16Type
instance IsType Int32  where typeRef _ = FFI.int32Type
instance IsType Int64  where typeRef _ = FFI.int64Type

-- Sequence types
instance (IsTypeNumber n, IsSized a) => IsType (Array n a)
    where typeRef _ = FFI.arrayType (typeRef (undefined :: a))
    	  	      		    (typeNumber (undefined :: n))

instance (IsTypeNumber n, IsPrimitive a) => IsType (Vector n a)
    where typeRef _ = FFI.arrayType (typeRef (undefined :: a))
    	  	      		    (typeNumber (undefined :: n))

instance (IsType a) => IsType (Ptr a) where
    typeRef ~(Ptr a) = FFI.pointerType (typeRef a) 0

-- Functions.
instance (IsFirstClass a, IsFunction b) => IsType (a->b) where
    typeRef = funcType []

instance (IsFirstClass a) => IsType (IO a) where
    typeRef = funcType []

--- Instances to classify types
instance IsArithmetic Float
instance IsArithmetic Double
instance IsArithmetic FP128
instance (IsTypeNumber n) => IsArithmetic (IntN n)
instance (IsTypeNumber n) => IsArithmetic (WordN n)
instance IsArithmetic Bool
instance IsArithmetic Int8
instance IsArithmetic Int16
instance IsArithmetic Int32
instance IsArithmetic Int64
instance IsArithmetic Word8
instance IsArithmetic Word16
instance IsArithmetic Word32
instance IsArithmetic Word64

instance IsFloating Float
instance IsFloating Double
instance IsFloating FP128

instance (IsTypeNumber n) => IsInteger (IntN n)
instance (IsTypeNumber n) => IsInteger (WordN n)
instance IsInteger Bool
instance IsInteger Int8
instance IsInteger Int16
instance IsInteger Int32
instance IsInteger Int64
instance IsInteger Word8
instance IsInteger Word16
instance IsInteger Word32
instance IsInteger Word64

instance IsFirstClass Float
instance IsFirstClass Double
instance IsFirstClass FP128
instance (IsTypeNumber n) => IsFirstClass (IntN n)
instance (IsTypeNumber n) => IsFirstClass (WordN n)
instance IsFirstClass Bool
instance IsFirstClass Int8
instance IsFirstClass Int16
instance IsFirstClass Int32
instance IsFirstClass Int64
instance IsFirstClass Word8
instance IsFirstClass Word16
instance IsFirstClass Word32
instance IsFirstClass Word64
instance (IsTypeNumber n, IsPrimitive a) => IsFirstClass (Vector n a)
instance (IsType a) => IsFirstClass (Ptr a)
instance IsFirstClass () -- XXX This isn't right, but () can be returned

instance (IsTypeNumber n) => IsSequence (Array n)
--instance (IsTypeNumber n, IsPrimitive a) => IsSequence (Vector n) a

instance IsSized Float
instance IsSized Double
instance IsSized FP128
instance (IsTypeNumber n) => IsSized (IntN n)
instance (IsTypeNumber n) => IsSized (WordN n)
instance IsSized Bool
instance IsSized Int8
instance IsSized Int16
instance IsSized Int32
instance IsSized Int64
instance IsSized Word8
instance IsSized Word16
instance IsSized Word32
instance IsSized Word64
instance (IsTypeNumber n, IsSized a) => IsSized (Array n a)
instance (IsTypeNumber n, IsPrimitive a) => IsSized (Vector n a)
instance (IsType a) => IsSized (Ptr a)

instance IsPrimitive Float
instance IsPrimitive Double
instance IsPrimitive FP128
instance (IsTypeNumber n) => IsPrimitive (IntN n)
instance (IsTypeNumber n) => IsPrimitive (WordN n)
instance IsPrimitive Bool
instance IsPrimitive Int8
instance IsPrimitive Int16
instance IsPrimitive Int32
instance IsPrimitive Int64
instance IsPrimitive Word8
instance IsPrimitive Word16
instance IsPrimitive Word32
instance IsPrimitive Word64
--instance IsPrimitive Label
instance IsPrimitive ()

-- Functions.
instance (IsFirstClass a, IsFunction b) => IsFunction (a->b) where
    funcType ts _ = funcType (typeRef (undefined :: a) : ts) (undefined :: b)
instance (IsFirstClass a) => IsFunction (IO a) where
    funcType ts _ = functionType False (typeRef (undefined :: a)) (reverse ts)

-- XXX Structures not implemented.  Tuples is probably an easy way.

