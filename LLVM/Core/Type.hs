{-# LANGUAGE
    DeriveDataTypeable
  , ExistentialQuantification
  , FunctionalDependencies
  , MultiParamTypeClasses
  #-}

module LLVM.Core.Type
    (
      Module(..)
    , withModule
    , ModuleProvider(..)
    , withModuleProvider

    -- * Types
    , Type(..)
    , TypeValue(..)
    , AnyType
    , HasAnyType(..)
    , DynamicType(..)
    , mkAnyType

    -- ** Integer types
    , Arithmetic
    , FirstClass
    , Primitive
    , Integer
    , integer
    , Int1(..)
    , int1
    , Int8(..)
    , int8
    , Int16(..)
    , int16
    , Int32(..)
    , int32
    , Int64(..)
    , int64
    , IntWidth(..)

    -- ** Real types
    , Real
    , Float(..)
    , float
    , Double(..)
    , double

    -- *** Machine-specific real types
    , X86Float80(..)
    , x86Float80
    , Float128(..)
    , float128
    , PPCFloat128(..)
    , ppcFloat128

    -- ** Array, pointer, and vector types
    , Sequence(..)
    , elementTypeDyn
    , Array(..)
    , array
    , arrayElementType
    , Pointer(..)
    , AddressSpace
    , addressSpace
    , fromAddressSpace
    , genericAddressSpace
    , pointerIn
    , pointer
    , pointerElementType
    , Vector(..)
    , vector
    , vectorElementType

    -- ** Function-related types
    , Function(..)
    , function
    , params
    , functionVarArg
    , isFunctionVarArg
    , getReturnType
    , getParamTypes

    -- *** Type hackery
    , functionParams
    , Params(..)
    , (:->)(..)
    , car
    , cdr

    -- ** Other types
    , Void(..)
    ) where

import Control.Applicative ((<$>))
import Data.Typeable (Typeable)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Array (allocaArray, peekArray, withArrayLen)
import Foreign.Marshal.Utils (fromBool, toBool)
import Prelude hiding (Double, Float, Integer, Real, mod)
import System.IO.Unsafe (unsafePerformIO)

import qualified LLVM.Core.FFI as FFI

import Debug.Trace


newtype Module = Module {
      fromModule :: ForeignPtr FFI.Module
    }
    deriving (Typeable)

withModule :: Module -> (FFI.ModuleRef -> IO a) -> IO a
withModule mod = withForeignPtr (fromModule mod)

newtype ModuleProvider = ModuleProvider {
      fromModuleProvider :: ForeignPtr FFI.ModuleProvider
    }
    deriving (Typeable)

withModuleProvider :: ModuleProvider -> (FFI.ModuleProviderRef -> IO a)
                   -> IO a
withModuleProvider prov = withForeignPtr (fromModuleProvider prov)

class Type t where
    typeRef :: t -> FFI.TypeRef
    anyType :: t -> AnyType

class Type t => TypeValue t where
    typeValue :: a -> t

class Type t => Arithmetic t
class Arithmetic t => Integer t
class Arithmetic t => Real t
class Type t => Primitive t

class FirstClass t
instance FirstClass AnyType

class HasAnyType t where
    fromAnyType :: AnyType -> t

data AnyType = forall t. Type t => AnyType t
               deriving (Typeable)

mkAnyType :: Type t => t -> AnyType
mkAnyType = AnyType

class Params a where
    toAnyList :: a -> [AnyType]
    fromAnyList :: [AnyType] -> (a, [AnyType])

instance Integer AnyType
instance Primitive AnyType

newtype Int1 = Int1 AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Integer, Primitive, Type,
              Typeable)

newtype Int8 = Int8 AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Integer, Primitive, Type,
              Typeable)

newtype Int16 = Int16 AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Integer, Primitive, Type,
              Typeable)

newtype Int32 = Int32 AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Integer, Primitive, Type,
              Typeable)

newtype Int64 = Int64 AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Integer, Primitive, Type,
              Typeable)

newtype IntWidth a = IntWidth AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Integer, Type, Typeable)

instance Real AnyType
instance Arithmetic AnyType

newtype Float = Float AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Primitive, Real, Type,
              Typeable)

newtype Double = Double AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Primitive, Real, Type,
              Typeable)

newtype X86Float80 = X86Float80 AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Primitive, Real, Type,
              Typeable)

newtype Float128 = Float128 AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Primitive, Real, Type,
              Typeable)

newtype PPCFloat128 = PPCFloat128 AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Primitive, Real, Type,
              Typeable)

class (Type a, Type t) => Sequence a t | a -> t where
    elementType :: a -> t

newtype Array a = Array AnyType
    deriving (HasAnyType, Type, Typeable)

arrayElementType :: Type t => Array t -> t
arrayElementType _ = undefined

newtype Pointer a = Pointer AnyType
    deriving (FirstClass, HasAnyType, Type, Typeable)

pointerElementType :: Pointer a -> a
pointerElementType _ = undefined

newtype Vector a = Vector AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Type, Typeable)

vectorElementType :: Vector t -> t
vectorElementType _ = undefined

newtype Void = Void AnyType
    deriving (HasAnyType, Type, Typeable)

class Type t => DynamicType t where
    toAnyType :: t              -- ^ not inspected
              -> AnyType

data Function r p = Function {
      fromNewFunction :: AnyType
    }
    deriving (Typeable)

functionParams :: Function r p -> p
functionParams _ = undefined

functionResult :: Function r p -> r
functionResult _ = undefined

data a :-> b = a :-> b
infixr 6 :->

car :: (a :-> b) -> a
car _ = undefined

cdr :: (a :-> b) -> b
cdr _ = undefined

int1 :: a -> Int1
int1 _ = Int1 $ mkAnyType FFI.int1Type

fromAny :: HasAnyType a => [AnyType] -> (a, [AnyType])
fromAny e | trace ("eee " ++ show (length e) ) False = undefined
fromAny (x:xs) = (fromAnyType x,xs)
fromAny _ = error "LLVM.Core.Type.fromAny: empty list"

int8 :: a -> Int8
int8 _ = Int8 $ mkAnyType FFI.int8Type

int16 :: a -> Int16
int16 _ = Int16 $ mkAnyType FFI.int16Type

int32 :: a -> Int32
int32 _ = Int32 $ mkAnyType FFI.int32Type

int64 :: a -> Int64
int64 _ = Int64 $ mkAnyType FFI.int64Type

integer :: Int -> b -> IntWidth a
integer width _ = IntWidth . mkAnyType . FFI.integerType $ fromIntegral width

-- Not possible:
--
-- instance Params (IntWidth a) where
--     toAnyList a = [toAnyType a]
--
-- instance DynamicType (IntWidth a) where
--     toAnyType _ = mkAnyType integerType

float :: a -> Float
float _ = Float $ mkAnyType FFI.floatType

double :: a -> Double
double _ = Double $ mkAnyType FFI.doubleType

x86Float80 :: a -> X86Float80
x86Float80 _ = X86Float80 $ mkAnyType FFI.x86FP80Type

float128 :: a -> Float128
float128 _ = Float128 $ mkAnyType FFI.fp128Type

ppcFloat128 :: a -> PPCFloat128
ppcFloat128 _ = PPCFloat128 $ mkAnyType FFI.ppcFP128Type

void :: a -> Void
void _ = Void $ mkAnyType FFI.voidType

functionType :: Bool -> AnyType -> [AnyType] -> AnyType
functionType varargs retType paramTypes = unsafePerformIO $
    withArrayLen (map typeRef paramTypes) $ \len ptr ->
        return . mkAnyType $ FFI.functionType (typeRef retType) ptr
                                        (fromIntegral len) (fromBool varargs)

params :: Params p => Function r p -> p
params f = case fromAnyList . toAnyList . functionParams $ f of
             (p, []) -> p
             _ -> error "LLVM.Core.Type.newParams: incompletely consumed params"

function :: (DynamicType r, Params p) => r -> p -> Function r p
function r p = Function . functionType False (toAnyType r) $ toAnyList p
    
functionVarArg :: (DynamicType r, Params p) => r -> p -> Function r p
functionVarArg r p = Function . functionType True (toAnyType r) $ toAnyList p
    
isFunctionVarArg :: Function r p -> Bool
isFunctionVarArg = toBool . FFI.isFunctionVarArg . typeRef

getReturnType :: (Params p) => Function r p -> AnyType
getReturnType = mkAnyType . FFI.getReturnType . typeRef

getParamTypes :: (Params p) => Function r p -> [AnyType]
getParamTypes typ = unsafePerformIO $ do
    let typ' = typeRef typ
        count = FFI.countParamTypes typ'
        len = fromIntegral count
    allocaArray len $ \ptr -> do
      FFI.getParamTypes typ' ptr
      map mkAnyType <$> peekArray len ptr

array :: (DynamicType t) => t -> Int -> Array t
array typ len = Array . mkAnyType $ FFI.arrayType (typeRef (toAnyType typ)) (fromIntegral len)

newtype AddressSpace = AddressSpace {
      fromAddressSpace :: Int
    }
    deriving (Eq, Ord, Show, Read)

addressSpace :: Int -> AddressSpace
addressSpace = AddressSpace

genericAddressSpace :: AddressSpace
genericAddressSpace = addressSpace 0

pointerIn :: (DynamicType t) => t -> AddressSpace -> Pointer t
pointerIn typ space = Pointer . mkAnyType $ FFI.pointerType (typeRef (toAnyType typ)) (fromIntegral . fromAddressSpace $ space)

pointer :: (DynamicType t) => t -> Pointer t
pointer typ = pointerIn typ genericAddressSpace

vector :: (DynamicType t, Primitive t) => t -> Int -> Vector t
vector typ len = Vector . mkAnyType $ FFI.vectorType (typeRef (toAnyType typ)) (fromIntegral len)

elementTypeDyn :: Type a => a -> AnyType
elementTypeDyn = mkAnyType . FFI.getElementType . typeRef

--
--
-- Handcrafted typeclass instances.
--
--

instance Eq AnyType where
    a == b = typeRef a == typeRef b

--
-- DynamicType
--

instance DynamicType AnyType where
    toAnyType = id

instance DynamicType Int1 where
    toAnyType = mkAnyType . int1

instance DynamicType Int8 where
    toAnyType = mkAnyType . int8

instance DynamicType Int16 where
    toAnyType = mkAnyType . int16

instance DynamicType Int32 where
    toAnyType = mkAnyType . int32

instance DynamicType Int64 where
    toAnyType = mkAnyType . int64

instance DynamicType Float where
    toAnyType = mkAnyType . float

instance DynamicType Double where
    toAnyType = mkAnyType . double

instance DynamicType Float128 where
    toAnyType = mkAnyType . float128

instance DynamicType PPCFloat128 where
    toAnyType = mkAnyType . ppcFloat128

instance DynamicType Void where
    toAnyType = mkAnyType . void

instance DynamicType X86Float80 where
    toAnyType = mkAnyType . x86Float80

instance (DynamicType r, Params p) => DynamicType (Function r p) where
    toAnyType f = let parms = toAnyList . functionParams $ f
                      ret = toAnyType . functionResult $ f
                  in functionType False ret parms

instance (DynamicType t) => DynamicType (Array t) where
    toAnyType = mkAnyType . flip array 0 . toAnyType . arrayElementType

instance (DynamicType t) => DynamicType (Pointer t) where
    toAnyType = mkAnyType . pointer . toAnyType . pointerElementType

instance (DynamicType t) => DynamicType (Vector t) where
    toAnyType = mkAnyType . flip vector 1 . toAnyType . vectorElementType

--
-- HasAnyType
--

instance HasAnyType AnyType where
    fromAnyType = id

instance HasAnyType (Function r p) where
    fromAnyType = Function

--
-- Params
--

instance Params () where
    toAnyList _ = []
    fromAnyList xs = ((), xs)

instance Params Int1 where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance Params Int8 where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance Params Int16 where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance Params Int32 where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance Params Int64 where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance Params Float where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance Params Double where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance Params X86Float80 where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance Params Float128 where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance Params PPCFloat128 where
    toAnyList a = [toAnyType a]
    fromAnyList  = fromAny

instance Params Void where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance Params AnyType where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance (DynamicType a, HasAnyType a, Params b) => Params (a :-> b) where
    toAnyList a = toAnyType (car a) : toAnyList (cdr a)
    fromAnyList (x:xs) = let (y,ys) = fromAnyList xs
                         in (fromAnyType x :-> y,ys)
    fromAnyList _ = error "LLVM.Core.Type.fromAnyList(:->): empty list"

instance DynamicType p => Params (Function r p) where
    toAnyList a = [toAnyType (functionParams a)]
    fromAnyList = fromAny

instance (DynamicType t) => Params (Pointer t) where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance (DynamicType t) => Params (Vector t) where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

--
-- Sequence
--

instance Sequence AnyType AnyType where
    elementType = elementTypeDyn

instance Type t => Sequence (Array t) t where
    elementType = arrayElementType

instance Type t => Sequence (Pointer t) t where
    elementType = pointerElementType

instance Type t => Sequence (Vector t) t where
    elementType = vectorElementType

--
-- Show
--

instance Show (IntWidth a) where show _ = "IntWidth"
instance Show AnyType where show a = "AnyType " ++ show (typeRef a)
instance Show Double where show _ = "Double"
instance Show Float where show _ = "Float"
instance Show Float128 where show _ = "Float128"
instance Show Int1 where show _ = "Int1"
instance Show Int16 where show _ = "Int16"
instance Show Int32 where show _ = "Int32"
instance Show Int64 where show _ = "Int64"
instance Show Int8 where show _ = "Int8"
instance Show PPCFloat128 where show _ = "PPCFloat128"
instance Show Void where show _ = "Void"
instance Show X86Float80 where show _ = "X86Float80"
instance (Show t, Type t) => Show (Array t) where show a = "Array " ++ show (arrayElementType a)
instance (Show t, Type t) => Show (Pointer t) where show a = "Pointer " ++ show (pointerElementType a)
instance (Show a) => Show (Vector a) where show a = "Vector " ++ show (vectorElementType a)
instance (Show r, Show p, Params p) => Show (Function r p) where show a = "Function " ++ show (functionResult a) ++ " " ++ show (params a)
instance (Show a, Show b) => Show (a :-> b) where show a = show (car a) ++ " :-> " ++ show (cdr a)

--
-- Type
--

instance Type FFI.TypeRef where
    anyType = AnyType
    typeRef = id

instance Type AnyType where
    typeRef (AnyType a) = typeRef a
    anyType = id

instance Type (Function r p) where
    typeRef = typeRef . fromNewFunction
    anyType = fromNewFunction


--
-- TypeValue
--

--instance TypeValue AnyType where
--    typeValue = const

instance TypeValue Int1 where
    typeValue = int1

instance TypeValue Int8 where
    typeValue = int8

instance TypeValue Int16 where
    typeValue = int16

instance TypeValue Int32 where
    typeValue = int32

instance TypeValue Int64 where
    typeValue = int64

instance TypeValue Float where
    typeValue = float

instance TypeValue Double where
    typeValue = double

instance TypeValue Float128 where
    typeValue = float128

instance TypeValue PPCFloat128 where
    typeValue = ppcFloat128

instance TypeValue Void where
    typeValue = void

instance TypeValue X86Float80 where
    typeValue = x86Float80

instance (DynamicType r, Params p) => TypeValue (Function r p) where
    typeValue _ = function undefined undefined

instance (DynamicType t) => TypeValue (Array t) where
    typeValue _ = array undefined 0

instance (DynamicType t) => TypeValue (Pointer t) where
    typeValue _ = pointer undefined

instance (DynamicType t, Primitive t) => TypeValue (Vector t) where
    typeValue _= vector undefined 1
