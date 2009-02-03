{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, FlexibleInstances, FlexibleContexts, UndecidableInstances, IncoherentInstances #-}
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
    -- ** Others
    IsPowerOf2,
    -- ** Type tests
    TypeDesc(..),
    isFloating,
    isSigned,
    typeRef,
    typeName
    ) where
import Data.List(intercalate)
import Data.Int
import Data.Word
import Data.TypeLevel hiding (Bool, Eq)
import LLVM.Core.Util(functionType)
import LLVM.Core.Data
import qualified LLVM.FFI.Core as FFI

-- XXX
-- IsSized should have two parameters, the second being the size and computed from the first

-- Usage: vector precondition
-- XXX This could defined inductively, but this is good enough for LLVM
class (Pos n) => IsPowerOf2 n
instance (LogBase D2 n l, Pos n) => IsPowerOf2 n

-- TODO:
-- Move IntN, WordN to a special module that implements those types
--   properly in Haskell.
-- Also more Array and Vector to a Haskell module to implement them.
-- Add Label?
-- Add structures (using tuples, maybe nested).

-- |The 'IsType' class classifies all types that have an LLVM representation.
class IsType a where
    typeDesc :: a -> TypeDesc

typeRef :: (IsType a) => a -> FFI.TypeRef  -- ^The argument is never evaluated
typeRef = code . typeDesc
  where code TDFloat  = FFI.floatType
  	code TDDouble = FFI.doubleType
	code TDFP128  = FFI.fp128Type
	code TDVoid   = FFI.voidType
	code (TDInt _ n)  = FFI.integerType (fromInteger n)
	code (TDArray n a) = FFI.arrayType (code a) (fromInteger n)
	code (TDVector n a) = FFI.vectorType (code a) (fromInteger n)
	code (TDPtr a) = FFI.pointerType (code a) 0
	code (TDFunction as b) = functionType False (code b) (map code as)

typeName :: (IsType a) => a -> String
typeName = code . typeDesc
  where code TDFloat  = "f32"
  	code TDDouble = "f64"
	code TDFP128  = "f128"
	code TDVoid   = "void"
	code (TDInt _ n)  = "i" ++ show n
	code (TDArray n a) = "[" ++ show n ++ " x " ++ code a ++ "]"
	code (TDVector n a) = "<" ++ show n ++ " x " ++ code a ++ ">"
	code (TDPtr a) = code a ++ "*"
	code (TDFunction as b) = code b ++ "(" ++ intercalate "," (map code as) ++ ")"

-- |Type descriptor, used to convey type information through the LLVM API.
data TypeDesc = TDFloat | TDDouble | TDFP128 | TDVoid | TDInt Bool Integer
              | TDArray Integer TypeDesc | TDVector Integer TypeDesc
	      | TDPtr TypeDesc | TDFunction [TypeDesc] TypeDesc
    deriving (Eq, Ord, Show)

-- XXX isFloating and typeName could be extracted from typeRef
-- Usage:
--   superclass of IsConst
--   add, sub, mul, neg context
--   used to get type name to call intrinsic
-- |Arithmetic types, i.e., integral and floating types.
class IsFirstClass a => IsArithmetic a

-- Usage:
--  constI, allOnes
--  many instructions.  XXX some need vector
--  used to find signedness in Arithmetic
-- |Integral types.
class IsArithmetic a => IsInteger a

isSigned :: (IsInteger a) => a -> Bool
isSigned = is . typeDesc
  where is (TDInt s _) = s
  	is (TDVector _ a) = is a
	is _ = error "isSigned got impossible input"

-- Usage:
--  constF
--  many instructions
-- |Floating types.
class IsArithmetic a => IsFloating a

isFloating :: (IsArithmetic a) => a -> Bool
isFloating = is . typeDesc
  where is TDFloat = True
  	is TDDouble = True
	is TDFP128 = True
	is (TDVector _ a) = is a
	is _ = False

-- Usage:
--  Precondition for Vector
-- |Primitive types.
class IsType a => IsPrimitive a

-- Usage:
--  Precondition for function args and result.
--  Used by some instructions, like ret and phi.
--  XXX IsSized as precondition?
-- |First class types, i.e., the types that can be passed as arguments, etc.
class IsType a => IsFirstClass a

-- Usage:
--  Context for Array being a type
--  thus, allocation instructions
-- |Types with a fixed size.
class (IsType a) => IsSized a

-- |Function type.
class (IsType a) => IsFunction a where
    funcType :: [TypeDesc] -> a -> TypeDesc

-- Only make instances for types that make sense in Haskell
-- (i.e., some floating types are excluded).

-- Floating point types.
instance IsType Float  where typeDesc _ = TDFloat
instance IsType Double where typeDesc _ = TDDouble
instance IsType FP128  where typeDesc _ = TDFP128

-- Void type
instance IsType ()     where typeDesc _ = TDVoid

-- Variable size integer types
instance (Pos n) => IsType (IntN n)
    where typeDesc _ = TDInt True  (toNum (undefined :: n))

instance (Pos n) => IsType (WordN n)
    where typeDesc _ = TDInt False (toNum (undefined :: n))

-- Fixed size integer types.
instance IsType Bool   where typeDesc _ = TDInt False  1
instance IsType Word8  where typeDesc _ = TDInt False  8
instance IsType Word16 where typeDesc _ = TDInt False 16
instance IsType Word32 where typeDesc _ = TDInt False 32
instance IsType Word64 where typeDesc _ = TDInt False 64
instance IsType Int8   where typeDesc _ = TDInt True   8
instance IsType Int16  where typeDesc _ = TDInt True  16
instance IsType Int32  where typeDesc _ = TDInt True  32
instance IsType Int64  where typeDesc _ = TDInt True  64

-- Sequence types
instance (Nat n, IsSized a) => IsType (Array n a)
    where typeDesc _ = TDArray (toNum (undefined :: n))
    	  	               (typeDesc (undefined :: a))
instance (IsPowerOf2 n, IsPrimitive a) => IsType (Vector n a)
    where typeDesc _ = TDVector (toNum (undefined :: n))
    	  	       		(typeDesc (undefined :: a))

-- Pointer type.
instance (IsType a) => IsType (Ptr a) where
    typeDesc _ = TDPtr (typeDesc (undefined :: a))

-- Functions.
instance (IsFirstClass a, IsFunction b) => IsType (a->b) where
    typeDesc = funcType []

-- Function base type, always IO.
instance (IsFirstClass a) => IsType (IO a) where
    typeDesc = funcType []

--- Instances to classify types
instance IsArithmetic Float
instance IsArithmetic Double
instance IsArithmetic FP128
instance (Pos n) => IsArithmetic (IntN n)
instance (Pos n) => IsArithmetic (WordN n)
instance IsArithmetic Bool
instance IsArithmetic Int8
instance IsArithmetic Int16
instance IsArithmetic Int32
instance IsArithmetic Int64
instance IsArithmetic Word8
instance IsArithmetic Word16
instance IsArithmetic Word32
instance IsArithmetic Word64
instance (IsPowerOf2 n, IsPrimitive a, IsArithmetic a) => IsArithmetic (Vector n a)

instance IsFloating Float
instance IsFloating Double
instance IsFloating FP128
instance (IsPowerOf2 n, IsPrimitive a, IsFloating a) => IsFloating (Vector n a)

instance (Pos n) => IsInteger (IntN n)
instance (Pos n) => IsInteger (WordN n)
instance IsInteger Bool
instance IsInteger Int8
instance IsInteger Int16
instance IsInteger Int32
instance IsInteger Int64
instance IsInteger Word8
instance IsInteger Word16
instance IsInteger Word32
instance IsInteger Word64
instance (IsPowerOf2 n, IsPrimitive a, IsInteger a) => IsInteger (Vector n a)

instance IsFirstClass Float
instance IsFirstClass Double
instance IsFirstClass FP128
instance (Pos n) => IsFirstClass (IntN n)
instance (Pos n) => IsFirstClass (WordN n)
instance IsFirstClass Bool
instance IsFirstClass Int8
instance IsFirstClass Int16
instance IsFirstClass Int32
instance IsFirstClass Int64
instance IsFirstClass Word8
instance IsFirstClass Word16
instance IsFirstClass Word32
instance IsFirstClass Word64
instance (IsPowerOf2 n, IsPrimitive a) => IsFirstClass (Vector n a)
instance (IsType a) => IsFirstClass (Ptr a)
instance IsFirstClass () -- XXX This isn't right, but () can be returned

instance IsSized Float
instance IsSized Double
instance IsSized FP128
instance (Pos n) => IsSized (IntN n)
instance (Pos n) => IsSized (WordN n)
instance IsSized Bool
instance IsSized Int8
instance IsSized Int16
instance IsSized Int32
instance IsSized Int64
instance IsSized Word8
instance IsSized Word16
instance IsSized Word32
instance IsSized Word64
instance (Nat n, IsSized a) => IsSized (Array n a)
instance (IsPowerOf2 n, IsPrimitive a) => IsSized (Vector n a)
instance (IsType a) => IsSized (Ptr a)

instance IsPrimitive Float
instance IsPrimitive Double
instance IsPrimitive FP128
instance (Pos n) => IsPrimitive (IntN n)
instance (Pos n) => IsPrimitive (WordN n)
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
    funcType ts _ = funcType (typeDesc (undefined :: a) : ts) (undefined :: b)
instance (IsFirstClass a) => IsFunction (IO a) where
    funcType ts _ = TDFunction (reverse ts) (typeDesc (undefined :: a))

-- XXX Structures not implemented.  Tuples is probably an easy way.

