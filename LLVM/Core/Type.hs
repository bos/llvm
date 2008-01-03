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

class Type a where
    typeRef :: a -> FFI.TypeRef
    anyType :: a -> AnyType

class Type t => TypeValue t where
    typeValue :: a -> t

class Type a => Arithmetic a
class Arithmetic a => Integer a
class Arithmetic a => Real a

class FirstClass a
instance FirstClass AnyType

class HasAnyType a where
    fromAnyType :: AnyType -> a

instance Type FFI.TypeRef where
    anyType = AnyType
    typeRef = id

data AnyType = forall a. Type a => AnyType a
               deriving (Typeable)

instance Eq AnyType where
    a == b = typeRef a == typeRef b

instance Show AnyType where
    show a = "AnyType " ++ show (typeRef a)

mkAnyType :: Type a => a -> AnyType
mkAnyType = AnyType

instance Type AnyType where
    typeRef (AnyType a) = typeRef a
    anyType = id

instance HasAnyType AnyType where
    fromAnyType = id

class Params a where
    toAnyList :: a -> [AnyType]
    fromAnyList :: [AnyType] -> (a, [AnyType])

instance Integer AnyType

newtype Int1 = Int1 AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Integer, Type, Typeable)

instance Show Int1 where
    show _ = "Int1"

newtype Int8 = Int8 AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Integer, Type, Typeable)

instance Show Int8 where
    show _ = "Int8"

newtype Int16 = Int16 AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Integer, Type, Typeable)

instance Show Int16 where
    show _ = "Int16"

newtype Int32 = Int32 AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Integer, Type, Typeable)

instance Show Int32 where
    show _ = "Int32"

newtype Int64 = Int64 AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Integer, Type, Typeable)

instance Show Int64 where
    show _ = "Int64"

newtype IntWidth a = IntWidth AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Integer, Type, Typeable)

instance Show (IntWidth a) where
    show _ = "IntWidth"

instance Real AnyType
instance Arithmetic AnyType

newtype Float = Float AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Real, Type, Typeable)

instance Show Float where
    show _ = "Float"

newtype Double = Double AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Real, Type, Typeable)

instance Show Double where
    show _ = "Double"

newtype X86Float80 = X86Float80 AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Real, Type, Typeable)

instance Show X86Float80 where
    show _ = "X86Float80"

newtype Float128 = Float128 AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Real, Type, Typeable)

instance Show Float128 where
    show _ = "Float128"

newtype PPCFloat128 = PPCFloat128 AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Real, Type, Typeable)

instance Show PPCFloat128 where
    show _ = "PPCFloat128"

class (Type a, Type t) => Sequence a t | a -> t where
    elementType :: a -> t

instance Sequence AnyType AnyType where
    elementType = elementTypeDyn

newtype Array a = Array AnyType
    deriving (HasAnyType, Type, Typeable)

arrayElementType :: Array a -> a
arrayElementType _ = undefined

instance Type a => Sequence (Array a) a where
    elementType = arrayElementType

instance (Show a) => Show (Array a) where
    show a = "Array " ++ show (arrayElementType a)

newtype Pointer a = Pointer AnyType
    deriving (FirstClass, HasAnyType, Type, Typeable)

pointerElementType :: Pointer a -> a
pointerElementType _ = undefined

instance Type a => Sequence (Pointer a) a where
    elementType = pointerElementType

instance (Show a) => Show (Pointer a) where
    show a = "Pointer " ++ show (pointerElementType a)

newtype Vector a = Vector AnyType
    deriving (Arithmetic, FirstClass, HasAnyType, Type, Typeable)

vectorElementType :: Vector a -> a
vectorElementType _ = undefined

instance Type a => Sequence (Vector a) a where
    elementType = vectorElementType

instance (Show a) => Show (Vector a) where
    show a = "Vector " ++ show (vectorElementType a)

newtype Void = Void AnyType
    deriving (HasAnyType, Type, Typeable)

instance Show Void where
    show _ = "Void"

class Type a => DynamicType a where
    toAnyType :: a              -- ^ not inspected
              -> AnyType

data Function r p = Function {
      fromNewFunction :: AnyType
    }
    deriving (Typeable)

instance HasAnyType (Function r p) where
    fromAnyType = Function

instance Type (Function r p) where
    typeRef = typeRef . fromNewFunction
    anyType = fromNewFunction

instance (Show r, Show p, Params p) => Show (Function r p) where
    show a = "Function " ++ show (functionResult a) ++ " " ++ show (params a)

functionParams :: Function r p -> p
functionParams _ = undefined

functionResult :: Function r p -> r
functionResult _ = undefined

instance (DynamicType r, Params p) => DynamicType (Function r p) where
    toAnyType f = let parms = toAnyList . functionParams $ f
                      ret = toAnyType . functionResult $ f
                  in functionType False ret parms

instance DynamicType AnyType where
    toAnyType = id

data a :-> b = a :-> b
infixr 6 :->

car :: (a :-> b) -> a
car _ = undefined

cdr :: (a :-> b) -> b
cdr _ = undefined

instance (Show a, Show b) => Show (a :-> b) where
    show a = show (car a) ++ " :-> " ++ show (cdr a)

int1 :: a -> Int1
int1 _ = Int1 $ mkAnyType FFI.int1Type

fromAny :: HasAnyType a => [AnyType] -> (a, [AnyType])
fromAny e | trace ("eee " ++ show (length e) ) False = undefined
fromAny (x:xs) = (fromAnyType x,xs)
fromAny _ = error "LLVM.Core.Type.fromAny: empty list"

instance Params () where
    toAnyList _ = []
    fromAnyList _ = error "fromAnyList ()"

instance Params Int1 where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance TypeValue Int1 where
    typeValue = int1

instance DynamicType Int1 where
    toAnyType = mkAnyType . int1

int8 :: a -> Int8
int8 _ = Int8 $ mkAnyType FFI.int8Type

instance Params Int8 where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance Params AnyType where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance DynamicType Int8 where
    toAnyType = mkAnyType . int8

int16 :: a -> Int16
int16 _ = Int16 $ mkAnyType FFI.int16Type

instance Params Int16 where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance DynamicType Int16 where
    toAnyType = mkAnyType . int16

int32 :: a -> Int32
int32 _ = Int32 $ mkAnyType FFI.int32Type

instance Params Int32 where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance DynamicType Int32 where
    toAnyType = mkAnyType . int32

int64 :: a -> Int64
int64 _ = Int64 $ mkAnyType FFI.int64Type

instance Params Int64 where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance DynamicType Int64 where
    toAnyType = mkAnyType . int64

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

instance Params Float where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance DynamicType Float where
    toAnyType = mkAnyType . float

double :: a -> Double
double _ = Double $ mkAnyType FFI.doubleType

instance Params Double where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance DynamicType Double where
    toAnyType = mkAnyType . double

x86Float80 :: a -> X86Float80
x86Float80 _ = X86Float80 $ mkAnyType FFI.x86FP80Type

instance Params X86Float80 where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance DynamicType X86Float80 where
    toAnyType = mkAnyType . x86Float80

float128 :: a -> Float128
float128 _ = Float128 $ mkAnyType FFI.fp128Type

instance Params Float128 where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance DynamicType Float128 where
    toAnyType = mkAnyType . float128

ppcFloat128 :: a -> PPCFloat128
ppcFloat128 _ = PPCFloat128 $ mkAnyType FFI.ppcFP128Type

instance Params PPCFloat128 where
    toAnyList a = [toAnyType a]
    fromAnyList  = fromAny

instance DynamicType PPCFloat128 where
    toAnyType = mkAnyType . ppcFloat128

void :: a -> Void
void _ = Void $ mkAnyType FFI.voidType

instance Params Void where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

instance DynamicType Void where
    toAnyType = mkAnyType . void

instance (DynamicType a, HasAnyType a, Params b) => Params (a :-> b) where
    toAnyList a = toAnyType (car a) : toAnyList (cdr a)
    fromAnyList (x:xs) = let (y,ys) = fromAnyList xs
                         in (fromAnyType x :-> y,ys)
    fromAnyList _ = error "LLVM.Core.Type.fromAnyList(:->): empty list"

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

instance DynamicType p => Params (Function r p) where
    toAnyList a = [toAnyType (functionParams a)]
    fromAnyList = fromAny
    
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

instance (DynamicType t) => DynamicType (Array t) where
    toAnyType = mkAnyType . flip array 0 . toAnyType . arrayElementType

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

instance (DynamicType t) => DynamicType (Pointer t) where
    toAnyType = mkAnyType . pointer . toAnyType . pointerElementType

instance (DynamicType t) => Params (Pointer t) where
    toAnyList a = [toAnyType a]
    fromAnyList = fromAny

vector :: (DynamicType t) => t -> Int -> Vector t
vector typ len = Vector . mkAnyType $ FFI.vectorType (typeRef (toAnyType typ)) (fromIntegral len)

instance (DynamicType t) => DynamicType (Vector t) where
    toAnyType = mkAnyType . flip vector 0 . toAnyType . vectorElementType

elementTypeDyn :: Type a => a -> AnyType
elementTypeDyn = mkAnyType . FFI.getElementType . typeRef
