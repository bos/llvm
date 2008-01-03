{-# LANGUAGE
    DeriveDataTypeable
  , ExistentialQuantification
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

    , Instruction(..)

    -- * Constants
    , ConstInt(..)
    , ConstReal(..)
    , ConstArray(..)

    -- ** Useful functions
    , params
    , getValueName
    , setValueName
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

instance DynamicValue AnyValue where
    fromAnyValue = id

instance Value FFI.ValueRef where
    valueRef = id
    anyValue = AnyValue

mkAnyValue :: Value a => a -> AnyValue
mkAnyValue = AnyValue

class Value a => ConstValue a
class Value a => Arithmetic a
class Arithmetic a => Integer a
class Arithmetic a => Real a
class Arithmetic a => Vector a
class ConstValue a => GlobalValue a
class GlobalValue a => GlobalVariable a

instance Value AnyValue where
    valueRef (AnyValue a) = valueRef a
    anyValue = id

instance ConstValue AnyValue
instance GlobalValue AnyValue
instance GlobalVariable AnyValue
instance Arithmetic AnyValue
instance Integer AnyValue
instance Real AnyValue

getValueName :: Value v => v -> IO (Maybe String)
getValueName v = do
  namePtr <- FFI.getValueName (valueRef v)
  if namePtr == nullPtr
    then return Nothing
    else Just <$> peekCString namePtr

setValueName :: Value v => v -> String -> IO ()
setValueName v name = withCString name (FFI.setValueName (valueRef v))

dumpValue :: Value v => v -> IO ()
dumpValue = FFI.dumpValue . valueRef

newtype Instruction a = Instruction AnyValue
    deriving (DynamicValue, Typeable, Value)

newtype Global t = Global AnyValue
    deriving (ConstValue, DynamicValue, GlobalValue, Typeable, Value)

newtype GlobalVar t = GlobalVar AnyValue
    deriving (ConstValue, DynamicValue, GlobalValue, GlobalVariable,
              Typeable, Value)

fromAny :: T.Type t => t -> [AnyValue] -> (Argument t, [AnyValue])
fromAny _ (x:xs) = (Argument x,xs)
fromAny _ _ = error "LLVM.Core.Value.fromAny: empty list"

globalVarType :: GlobalVar t -> t
globalVarType _ = undefined

instance T.Type t => TypedValue (GlobalVar t) t where
    typeOf = globalVarType

newtype Function t = Function AnyValue
    deriving (ConstValue, DynamicValue, GlobalValue, GlobalVariable,
              Typeable, Value)

newtype Argument t = Argument AnyValue
    deriving (DynamicValue, Typeable, Value)

instance T.Params p => TypedValue (Function p) (T.Function p) where
    typeOf _ = T.function undefined

instance (Params b c) => Params (a :-> b) (Argument a :-> c) where
    fromAnyList a (x:xs) = let (y,ys) = fromAnyList (T.cdr a) xs
                           in (Argument x :-> y,ys)
    fromAnyList _ _ = error "LLVM.Core.Value.fromAnyList(:->): empty list"

newtype ConstInt t = ConstInt AnyValue
    deriving (Arithmetic, ConstValue, DynamicValue, Integer, Typeable, Value)

instance TypedValue (ConstInt T.Int1) T.Int1 where
    typeOf = T.int1

instance TypedValue (Argument T.Int1) T.Int1 where
    typeOf = T.int1

instance Params T.Int1 (Argument T.Int1) where
    fromAnyList = fromAny

instance TypedValue (ConstInt T.Int8) T.Int8 where
    typeOf = T.int8

instance TypedValue (Argument T.Int8) T.Int8 where
    typeOf = T.int8

-- instance Params T.Int8 where
--     fromAnyList = fromAny

instance TypedValue (ConstInt T.Int16) T.Int16 where
    typeOf = T.int16

instance TypedValue (Argument T.Int16) T.Int16 where
    typeOf = T.int16

-- instance Params T.Int16 where
--     fromAnyList = fromAny

instance TypedValue (ConstInt T.Int32) T.Int32 where
    typeOf = T.int32

instance TypedValue (Argument T.Int32) T.Int32 where
    typeOf = T.int32

instance TypedValue (Instruction T.Int32) T.Int32 where
    typeOf = T.int32

instance Params T.Int32 (Argument T.Int32) where
    fromAnyList = fromAny

instance TypedValue (ConstInt T.Int64) T.Int64 where
    typeOf = T.int64

instance TypedValue (Argument T.Int64) T.Int64 where
    typeOf = T.int64

-- instance Params T.Int64 where
--     fromAnyList = fromAny

newtype ConstArray t = ConstArray AnyValue
    deriving (ConstValue, DynamicValue, Typeable, Value)

instance (T.DynamicType a) => TypedValue (ConstArray a) (T.Array a) where
    typeOf _ = T.array undefined 0

newtype ConstReal t = ConstReal AnyValue
    deriving (Arithmetic, ConstValue, DynamicValue, Real, Typeable, Value)

instance TypedValue (ConstReal T.Float) T.Float where
    typeOf = T.float

instance TypedValue (Argument T.Float) T.Float where
    typeOf = T.float

-- instance Params T.Float where
--     fromAnyList = fromAny

instance TypedValue (ConstReal T.Double) T.Double where
    typeOf = T.double

instance TypedValue (Argument T.Double) T.Double where
    typeOf = T.double

-- instance Params T.Double where
--     fromAnyList = fromAny

instance TypedValue (ConstReal T.X86Float80) T.X86Float80 where
    typeOf = T.x86Float80

instance TypedValue (Argument T.X86Float80) T.X86Float80 where
    typeOf = T.x86Float80

-- instance Params T.X86Float80 where
--     fromAnyList = fromAny

instance TypedValue (ConstReal T.Float128) T.Float128 where
    typeOf = T.float128

instance TypedValue (Argument T.Float128) T.Float128 where
    typeOf = T.float128

-- instance Params T.Float128 where
--     fromAnyList = fromAny

instance TypedValue (ConstReal T.PPCFloat128) T.PPCFloat128 where
    typeOf = T.ppcFloat128

instance TypedValue (Argument T.PPCFloat128) T.PPCFloat128 where
    typeOf = T.ppcFloat128

-- instance Params T.PPCFloat128 where
--     fromAnyList = fromAny

countParams :: Function p -> Int
countParams = fromIntegral . FFI.countParams . valueRef

listParams :: Function p -> [AnyValue]
listParams f = unsafePerformIO $ do
  let len = countParams f
  allocaArray len $ \ptr -> do
    FFI.getParams (valueRef f) ptr
    map mkAnyValue <$> peekArray len ptr

params :: (T.Params p, Params p v) => Function p -> v
params f = case fromAnyList paramTypes paramValues of
             (p, []) -> p
             _ -> error "incompletely consumed params"
    where paramValues = listParams f ++ [anyValue f]
          paramTypes = T.params (typeOf f)

typeOfDyn :: Value a => a -> T.AnyType
typeOfDyn val = unsafePerformIO $ T.mkAnyType <$> FFI.typeOf (valueRef val)
