{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, IncoherentInstances, TypeOperators, DeriveDataTypeable #-}
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
    IsIntegerOrPointer,
    IsFloating,
    IsPrimitive,
    IsFirstClass,
    IsSized,
    IsFunction,
    -- ** Structs
    (:&), (&),
    -- ** Type tests
    TypeDesc(..),
    isFloating,
    isSigned,
    typeRef,
    typeName,
    VarArgs, CastVarArgs,
    ) where
import Data.Typeable
import Data.List(intercalate)
import Data.Int
import Data.Word
import Data.TypeLevel hiding (Bool, Eq)
import Foreign.StablePtr (StablePtr, )
import LLVM.Core.Util(functionType, structType)
import LLVM.Core.Data
import qualified LLVM.FFI.Core as FFI

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
	code (TDFunction va as b) = functionType va (code b) (map code as)
	code TDLabel = FFI.labelType
        code (TDStruct ts packed) = structType (map code ts) packed

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
	code (TDFunction _ as b) = code b ++ "(" ++ intercalate "," (map code as) ++ ")"
        code TDLabel = "label"
        code (TDStruct as packed) = (if packed then "<{" else "{") ++
                                    intercalate "," (map code as) ++
                                    (if packed then "}>" else "}")

-- |Type descriptor, used to convey type information through the LLVM API.
data TypeDesc = TDFloat | TDDouble | TDFP128 | TDVoid | TDInt Bool Integer
              | TDArray Integer TypeDesc | TDVector Integer TypeDesc
	      | TDPtr TypeDesc | TDFunction Bool [TypeDesc] TypeDesc | TDLabel
              | TDStruct [TypeDesc] Bool
    deriving (Eq, Ord, Show, Typeable)

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
class (IsArithmetic a, IsIntegerOrPointer a) => IsInteger a

-- Usage:
--  icmp
-- |Integral or pointer type.
class IsIntegerOrPointer a

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
class (IsType a, Pos s) => IsSized a s | a -> s

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

-- Label type
instance IsType Label  where typeDesc _ = TDLabel

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
instance (Nat n, IsSized a s) => IsType (Array n a)
    where typeDesc _ = TDArray (toNum (undefined :: n))
    	  	               (typeDesc (undefined :: a))
instance (Nat n, IsPrimitive a) => IsType (Vector n a)
    where typeDesc _ = TDVector (toNum (undefined :: n))
    	  	       		(typeDesc (undefined :: a))

-- Pointer type.
instance (IsType a) => IsType (Ptr a) where
    typeDesc _ = TDPtr (typeDesc (undefined :: a))

instance IsType (StablePtr a) where
    typeDesc _ = TDPtr (typeDesc (undefined :: Int8))
{-
    typeDesc _ = TDPtr TDVoid

List: Type.cpp:1311: static llvm::PointerType* llvm::PointerType::get(const llvm::Type*, unsigned int): Assertion `ValueType != Type::VoidTy && "Pointer to void is not valid, use sbyte* instead!"' failed.
-}


-- Functions.
instance (IsFirstClass a, IsFunction b) => IsType (a->b) where
    typeDesc = funcType []

-- Function base type, always IO.
instance (IsFirstClass a) => IsType (IO a) where
    typeDesc = funcType []

-- Struct types, basically a list of component types.
instance (StructFields a) => IsType (Struct a) where
    typeDesc ~(Struct a) = TDStruct (fieldTypes a) False

instance (StructFields a) => IsType (PackedStruct a) where
    typeDesc ~(PackedStruct a) = TDStruct (fieldTypes a) True

-- Use a nested tuples for struct fields.
class StructFields as where
    fieldTypes :: as -> [TypeDesc]

instance (IsSized a sa, StructFields as) => StructFields (a :& as) where
    fieldTypes ~(a, as) = typeDesc a : fieldTypes as
instance StructFields () where
    fieldTypes _ = []

-- An alias for pairs to make structs look nicer
infixr :&
type (:&) a as = (a, as)
infixr &
(&) :: a -> as -> a :& as
a & as = (a, as)

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
instance (Nat n, IsPrimitive a, IsArithmetic a) => IsArithmetic (Vector n a)

instance IsFloating Float
instance IsFloating Double
instance IsFloating FP128
instance (Nat n, IsPrimitive a, IsFloating a) => IsFloating (Vector n a)

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
instance (Nat n, IsPrimitive a, IsInteger a) => IsInteger (Vector n a)

instance (Pos n) => IsIntegerOrPointer (IntN n)
instance (Pos n) => IsIntegerOrPointer (WordN n)
instance IsIntegerOrPointer Bool
instance IsIntegerOrPointer Int8
instance IsIntegerOrPointer Int16
instance IsIntegerOrPointer Int32
instance IsIntegerOrPointer Int64
instance IsIntegerOrPointer Word8
instance IsIntegerOrPointer Word16
instance IsIntegerOrPointer Word32
instance IsIntegerOrPointer Word64
instance (Nat n, IsPrimitive a, IsInteger a) => IsIntegerOrPointer (Vector n a)
instance (IsType a) => IsIntegerOrPointer (Ptr a)

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
instance (Nat n, IsPrimitive a) => IsFirstClass (Vector n a)
instance (IsType a) => IsFirstClass (Ptr a)
instance IsFirstClass (StablePtr a)
instance IsFirstClass Label
instance IsFirstClass () -- XXX This isn't right, but () can be returned
instance (StructFields as) => IsFirstClass (Struct as)

instance IsSized Float D32
instance IsSized Double D64
instance IsSized FP128 D128
instance (Pos n) => IsSized (IntN n) n
instance (Pos n) => IsSized (WordN n) n
instance IsSized Bool D1
instance IsSized Int8 D8
instance IsSized Int16 D16
instance IsSized Int32 D32
instance IsSized Int64 D64
instance IsSized Word8 D8
instance IsSized Word16 D16
instance IsSized Word32 D32
instance IsSized Word64 D64
instance (Nat n, IsSized a s, Mul n s ns, Pos ns) => IsSized (Array n a) ns
instance (Nat n, IsPrimitive a, IsSized a s, Mul n s ns, Pos ns) => IsSized (Vector n a) ns
instance (IsType a) => IsSized (Ptr a) PtrSize
instance IsSized (StablePtr a) PtrSize
-- instance IsSized Label PtrSize -- labels are not quite first classed
-- We cannot compute the sizes statically :(
instance (StructFields as) => IsSized (Struct as) UnknownSize
instance (StructFields as) => IsSized (PackedStruct as) UnknownSize

type UnknownSize = D99   -- XXX this is wrong!
type PtrSize = D32   -- XXX this is wrong!

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
instance IsPrimitive Label
instance IsPrimitive ()

-- Functions.
instance (IsFirstClass a, IsFunction b) => IsFunction (a->b) where
    funcType ts _ = funcType (typeDesc (undefined :: a) : ts) (undefined :: b)
instance (IsFirstClass a) => IsFunction (IO a) where
    funcType ts _ = TDFunction False (reverse ts) (typeDesc (undefined :: a))
instance (IsFirstClass a) => IsFunction (VarArgs a) where
    funcType ts _ = TDFunction True  (reverse ts) (typeDesc (undefined :: a))

-- |The 'VarArgs' type is a placeholder for the real 'IO' type that
-- can be obtained with 'castVarArgs'.
data VarArgs a
    deriving (Typeable)
instance IsType (VarArgs a) where
    typeDesc _ = error "typeDesc: Dummy type VarArgs used incorrectly"

-- |Define what vararg types are permissible.
class CastVarArgs a b
instance (CastVarArgs b c) => CastVarArgs (a -> b) (a -> c)
instance CastVarArgs (VarArgs a) (IO a)
instance (IsFirstClass a, CastVarArgs (VarArgs b) c) => CastVarArgs (VarArgs b) (a -> c)




-- XXX Structures not implemented.  Tuples is probably an easy way.

