{-# LANGUAGE
    DeriveDataTypeable
  , ExistentialQuantification
  , FlexibleContexts
  , FunctionalDependencies
  , MultiParamTypeClasses
  , UndecidableInstances
  #-}

module LLVM.Core.Value
    (
    -- * Values

    -- * Opaque wrapper for LLVM's basic value type
      AnyValue
    , DynamicValue(..)
    , mkAnyValue
    , typeOfDyn

    -- ** Type classes
    , Value(..)
    , Params(..)
    , ConstValue
    , GlobalValue
    , GlobalVariable
    , Arithmetic
    , Integer
    , Real
    , Vector

    , Global(..)
    , GlobalVar(..)
    , Function(..)
    , TypedValue(..)
    , Argument(..)

    , Instruction(..)

    -- * Constants
    , ConstInt(..)
    , ConstReal(..)
    , ConstArray(..)

    -- ** Useful functions
    , params
    , getName
    , setName
    , dumpValue
    ) where

import Control.Applicative ((<$>))
import Data.Typeable (Typeable)
import Foreign.C.String (peekCString, withCString)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (nullPtr)
import Prelude hiding (Integer, Real)
import System.IO.Unsafe (unsafePerformIO)

import qualified LLVM.Core.FFI as FFI
import LLVM.Core.Type ((:->)(..))
import qualified LLVM.Core.Type as T

-- import Debug.Trace


class Value a where
    valueRef :: a -> FFI.ValueRef
    anyValue :: a -> AnyValue

class DynamicValue a where
    fromAnyValue :: AnyValue -> a

class Params t v | t -> v where
    fromAnyList :: t -> [AnyValue] -> (v, [AnyValue])

-- | Recover the type of a value in a manner that preserves static
-- type safety.
class (T.Type t, Value v) => TypedValue v t | v -> t where
    typeOf :: v                 -- ^ value is not inspected
           -> t

data AnyValue = forall a. Value a => AnyValue a
                deriving (Typeable)

mkAnyValue :: Value a => a -> AnyValue
mkAnyValue = AnyValue

class Value a => ConstValue a
class Value a => Arithmetic a
class Arithmetic a => Integer a
class Arithmetic a => Real a
class Arithmetic a => Vector a
class ConstValue a => GlobalValue a
class GlobalValue a => GlobalVariable a

instance ConstValue AnyValue
instance GlobalValue AnyValue
instance GlobalVariable AnyValue
instance Arithmetic AnyValue
instance Integer AnyValue
instance Real AnyValue

getName :: Value v => v -> IO String
getName v = do
  namePtr <- FFI.getValueName (valueRef v)
  if namePtr == nullPtr
    then return []
    else peekCString namePtr

setName :: Value v => v -> String -> IO ()
setName v name = withCString name (FFI.setValueName (valueRef v))

dumpValue :: Value v => v -> IO ()
dumpValue = FFI.dumpValue . valueRef

newtype Instruction a = Instruction AnyValue
    deriving (DynamicValue, Typeable, Value)

newtype Global t = Global AnyValue
    deriving (ConstValue, DynamicValue, GlobalValue, Typeable, Value)

newtype GlobalVar t = GlobalVar AnyValue
    deriving (ConstValue, DynamicValue, GlobalValue, GlobalVariable,
              Typeable, Value)

fromAny :: (DynamicValue v, TypedValue v t, T.Type t) => t -> [AnyValue] -> (v, [AnyValue])
fromAny _ (x:xs) = (fromAnyValue x,xs)
fromAny _ _ = error "LLVM.Core.Value.fromAny: empty list"

globalVarType :: GlobalVar t -> t
globalVarType _ = undefined

data Function r p = Function {
      fromFunction :: AnyValue
    }
    deriving (Typeable)

instance ConstValue (Function r p)
instance GlobalValue (Function r p)
instance GlobalVariable (Function r p)

newtype Argument t = Argument AnyValue
    deriving (DynamicValue, Typeable, Value)

newtype ConstInt t = ConstInt AnyValue
    deriving (Arithmetic, ConstValue, DynamicValue, Integer, Typeable, Value)

newtype ConstArray t = ConstArray AnyValue
    deriving (ConstValue, DynamicValue, Typeable, Value)

newtype ConstReal t = ConstReal AnyValue
    deriving (Arithmetic, ConstValue, DynamicValue, Real, Typeable, Value)

countParams :: Function r p -> Int
countParams = fromIntegral . FFI.countParams . valueRef

listParams :: Function r p -> [AnyValue]
listParams f = unsafePerformIO $ do
  let len = countParams f
  allocaArray len $ \ptr -> do
    FFI.getParams (valueRef f) ptr
    map mkAnyValue <$> peekArray len ptr

params :: (T.DynamicType r, T.Params p, Params p v) => Function r p -> v
params f = case fromAnyList (T.params (typeOf f)) (listParams f) of
             (p, []) -> p
             _ -> error "LLVM.Core.Value.params: incompletely consumed params"

typeOfDyn :: Value a => a -> T.AnyType
typeOfDyn val = unsafePerformIO $ T.mkAnyType <$> FFI.typeOf (valueRef val)

--
--
-- Handcrafted typeclass instances.
--
--

--
-- DynamicValue
--

instance DynamicValue AnyValue where
    fromAnyValue = id

instance DynamicValue (Function r p) where
    fromAnyValue = Function

--
-- Params
--

instance Params T.Int1 (Argument T.Int1) where fromAnyList = fromAny
instance Params T.Int8 (Argument T.Int8) where fromAnyList = fromAny
instance Params T.Int16 (Argument T.Int16) where fromAnyList = fromAny
instance Params T.Int32 (Argument T.Int32) where fromAnyList = fromAny
instance Params T.Int64 (Argument T.Int64) where fromAnyList = fromAny
instance Params T.Float (Argument T.Float) where fromAnyList = fromAny
instance Params T.Double (Argument T.Double) where fromAnyList = fromAny
instance Params T.Float128 (Argument T.Float128) where fromAnyList = fromAny
instance Params T.PPCFloat128 (Argument T.PPCFloat128) where fromAnyList = fromAny
instance Params T.X86Float80 (Argument T.X86Float80) where fromAnyList = fromAny

instance (Params b c) => Params (a :-> b) (Argument a :-> c) where
    fromAnyList t (x:xs) = let (y,ys) = fromAnyList (T.cdr t) xs
                           in (Argument x :-> y,ys)
    fromAnyList _ _ = error "LLVM.Core.Value.fromAnyList(:->): empty list"

instance (T.DynamicType t) => Params (T.Pointer t) (Instruction (T.Pointer t)) where fromAnyList = fromAny
instance (T.DynamicType t, T.Primitive t) => Params (T.Vector t) (Instruction (T.Vector t)) where fromAnyList = fromAny


--
-- TypedValue
--

instance TypedValue (Argument T.Int1) T.Int1 where typeOf = T.int1
instance TypedValue (Argument T.Int8) T.Int8 where typeOf = T.int8
instance TypedValue (Argument T.Int16) T.Int16 where typeOf = T.int16
instance TypedValue (Argument T.Int32) T.Int32 where typeOf = T.int32
instance TypedValue (Argument T.Int64) T.Int64 where typeOf = T.int64

instance TypedValue (Argument T.Float) T.Float where typeOf = T.float
instance TypedValue (Argument T.Double) T.Double where typeOf = T.double
instance TypedValue (Argument T.Float128) T.Float128 where typeOf = T.float128
instance TypedValue (Argument T.PPCFloat128) T.PPCFloat128 where typeOf = T.ppcFloat128
instance TypedValue (Argument T.X86Float80) T.X86Float80 where typeOf = T.x86Float80

instance TypedValue (ConstInt T.Int1) T.Int1 where typeOf = T.int1
instance TypedValue (ConstInt T.Int8) T.Int8 where typeOf = T.int8
instance TypedValue (ConstInt T.Int16) T.Int16 where typeOf = T.int16
instance TypedValue (ConstInt T.Int32) T.Int32 where typeOf = T.int32
instance TypedValue (ConstInt T.Int64) T.Int64 where typeOf = T.int64

instance TypedValue (ConstReal T.Float) T.Float where typeOf = T.float
instance TypedValue (ConstReal T.Double) T.Double where typeOf = T.double
instance TypedValue (ConstReal T.Float128) T.Float128 where typeOf = T.float128
instance TypedValue (ConstReal T.PPCFloat128) T.PPCFloat128 where typeOf = T.ppcFloat128
instance TypedValue (ConstReal T.X86Float80) T.X86Float80 where typeOf = T.x86Float80

instance TypedValue (Instruction T.Int1) T.Int1 where typeOf = T.int1
instance TypedValue (Instruction T.Int8) T.Int8 where typeOf = T.int8
instance TypedValue (Instruction T.Int16) T.Int16 where typeOf = T.int16
instance TypedValue (Instruction T.Int32) T.Int32 where typeOf = T.int32
instance TypedValue (Instruction T.Int64) T.Int64 where typeOf = T.int64

instance TypedValue (Instruction T.Float) T.Float where typeOf = T.float
instance TypedValue (Instruction T.Double) T.Double where typeOf = T.double
instance TypedValue (Instruction T.Float128) T.Float128 where typeOf = T.float128
instance TypedValue (Instruction T.PPCFloat128) T.PPCFloat128 where typeOf = T.ppcFloat128
instance TypedValue (Instruction T.X86Float80) T.X86Float80 where typeOf = T.x86Float80


instance (T.DynamicType r, T.Params p) => TypedValue (Function r p) (T.Function r p) where typeOf _ = T.function undefined undefined
instance (T.DynamicType t) => TypedValue (ConstArray t) (T.Array t) where typeOf _ = T.array undefined 0
instance (T.DynamicType t) => TypedValue (Instruction (T.Array t)) (T.Array t) where typeOf _ = T.array undefined 0
instance (T.DynamicType t) => TypedValue (Instruction (T.Pointer t)) (T.Pointer t) where typeOf _ = T.pointer undefined
instance (T.DynamicType t, T.Primitive t) => TypedValue (Instruction (T.Vector t)) (T.Vector t) where typeOf _ = T.vector undefined 1
instance (T.Type t) => TypedValue (GlobalVar t) t where typeOf = globalVarType

--
-- Value
--

instance Value AnyValue where
    valueRef (AnyValue a) = valueRef a
    anyValue = id

instance Value (Function r p) where
    valueRef = valueRef . anyValue
    anyValue = fromFunction

instance Value FFI.ValueRef where
    valueRef = id
    anyValue = AnyValue

