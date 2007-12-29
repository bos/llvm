module LLVM.Core
    (
    -- * Modules
      Module
    , createModule

    -- * Module providers
    , ModuleProvider
    , createModuleProviderForExistingModule

    -- * Types
    , AnyType
    , Any(..)
    , Type(..)
    , mkAny
    , addTypeName
    , deleteTypeName
    , getElementType

    -- ** Integer types
    , IntType
    , int1Type
    , int8Type
    , int16Type
    , int32Type
    , int64Type
    , integerType

    -- ** Real types
    , floatType
    , doubleType
    , x86FP80Type
    , fp128Type
    , ppcFP128Type

    -- ** Function types
    , FunctionType
    , ParamList(..)
    , Return
    , (:->)
    , functionType
    , functionTypeVarArg
    , isFunctionVarArg
    , getReturnType
    , getParamTypes

    -- ** Array, pointer, and vector types
    , ArrayType
    , PointerType
    , VectorType
    , arrayType
    , pointerType
    , vectorType

    -- ** Other types
    , VoidType
    , voidType

    -- * Values
    , Value
    , addGlobal
    , setInitializer
    , typeOf

    -- ** Operations on functions
    , addFunction
    , deleteFunction
    , getNamedFunction

    -- * Constants
    , Const(..)

    -- ** Scalar constants
    , constInt
    , constWord
    , constReal

    -- ** Composite constants
    , constString
    , constStringNul

    -- ** Constant expressions
    , constBitCast

    -- * Basic blocks
    , BasicBlock
    , appendBasicBlock
    , insertBasicBlock
    , deleteBasicBlock

    -- * Instruction building
    , Builder
    , createBuilder
    , positionBefore
    , positionAtEnd

    -- ** Memory
    , buildGEP

    -- ** Terminators
    , buildRet

    -- ** Miscellaneous instructions
    , buildCall
    ) where

import Control.Applicative ((<$>))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.String (withCString, withCStringLen)
import Foreign.Marshal.Array (allocaArray, peekArray, withArrayLen)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.ForeignPtr (FinalizerPtr, newForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, nullPtr)
import Prelude hiding (mod)
import System.IO.Unsafe (unsafePerformIO)

import qualified LLVM.Core.FFI as FFI
import LLVM.Core.Types (BasicBlock(..), Builder(..), Module(..),
                        ModuleProvider(..), AnyType, Type(..), Value(..),
                        withModule, mkAny, Any(..))
import LLVM.Core.Instances ()


createModule :: String -> IO Module
createModule name =
    withCString name $ \namePtr -> do
      ptr <- FFI.moduleCreateWithName namePtr
      final <- h2c_module FFI.disposeModule
      Module <$> newForeignPtr final ptr

foreign import ccall "wrapper" h2c_module
    :: (FFI.ModuleRef -> IO ()) -> IO (FinalizerPtr a)


createModuleProviderForExistingModule :: Module -> IO ModuleProvider
createModuleProviderForExistingModule mod =
    withModule mod $ \modPtr -> do
        ptr <- FFI.createModuleProviderForExistingModule modPtr
        final <- h2c_moduleProvider FFI.disposeModuleProvider
        ModuleProvider <$> newForeignPtr final ptr

foreign import ccall "wrapper" h2c_moduleProvider
    :: (FFI.ModuleProviderRef -> IO ()) -> IO (FinalizerPtr a)


addTypeName :: (Type t) => Module -> t -> String -> IO Bool
addTypeName mod typ name =
    withModule mod $ \modPtr ->
      withCString name $ \namePtr ->
        (/=0) <$> FFI.addTypeName modPtr namePtr (fromType typ)
                 
deleteTypeName :: Module -> String -> IO ()
deleteTypeName mod name =
    withModule mod $ \modPtr ->
      withCString name $ FFI.deleteTypeName modPtr

class Type a => SequentialType a where
    getElementType :: a -> AnyType
    getElementType = mkAny . FFI.getElementType . fromType

newtype IntType a = IntType AnyType
    deriving (Type, Any)

class TypeInstance a where
    typeInstance :: a -> AnyType

int1Type :: IntType Bool
int1Type = IntType $ mkAny FFI.int1Type

instance TypeInstance (IntType Bool) where
    typeInstance _ = toAny int1Type

int8Type :: IntType Int8
int8Type = IntType $ mkAny FFI.int8Type

instance TypeInstance (IntType Int8) where
    typeInstance _ = toAny int8Type

int16Type :: IntType Int16
int16Type = IntType $ mkAny FFI.int16Type

instance TypeInstance (IntType Int16) where
    typeInstance _ = toAny int16Type

int32Type :: IntType Int32
int32Type = IntType $ mkAny FFI.int32Type

instance TypeInstance (IntType Int32) where
    typeInstance _ = toAny int32Type

int64Type :: IntType Int64
int64Type = IntType $ mkAny FFI.int64Type

instance TypeInstance (IntType Int64) where
    typeInstance _ = toAny int64Type

data SomeWidth

integerType :: Int -> IntType SomeWidth
integerType = IntType . mkAny . FFI.integerType . fromIntegral

newtype RealType a = RealType AnyType
    deriving (Type, Any)

floatType :: RealType Float
floatType = RealType $ mkAny FFI.floatType

instance TypeInstance (RealType Float) where
    typeInstance _ = toAny floatType

doubleType :: RealType Double
doubleType = RealType $ mkAny FFI.doubleType

instance TypeInstance (RealType Double) where
    typeInstance _ = toAny doubleType

data X86FP80

x86FP80Type :: RealType X86FP80
x86FP80Type = RealType $ mkAny FFI.x86FP80Type

instance TypeInstance (RealType X86FP80) where
    typeInstance _ = toAny x86FP80Type

data FP128

fp128Type :: RealType FP128
fp128Type = RealType $ mkAny FFI.fp128Type

instance TypeInstance (RealType FP128) where
    typeInstance _ = toAny fp128Type

data PPCFP128

ppcFP128Type :: RealType PPCFP128
ppcFP128Type = RealType $ mkAny FFI.ppcFP128Type

instance TypeInstance (RealType PPCFP128) where
    typeInstance _ = toAny ppcFP128Type

newtype VoidType = VoidType AnyType
    deriving (Type, Any)

instance TypeInstance VoidType where
    typeInstance _ = toAny voidType

voidType :: VoidType
voidType = VoidType $ mkAny FFI.voidType

data a :-> b
infixr 6 :->

car :: (a :-> b) -> a
car _ = undefined

cdr :: (a :-> b) -> b
cdr _ = undefined

class ParamList l where
    listValue :: l -> [AnyType]

data Return a

ret :: Return a -> a
ret _ = undefined

instance TypeInstance a => ParamList (Return a) where
    listValue a = [typeInstance (ret a)]

instance (TypeInstance a, ParamList b) => ParamList (a :-> b) where
    listValue a = typeInstance (car a) : listValue (cdr a)

newtype FunctionType p = FunctionType AnyType
    deriving (Type, Any)

functionTypeInternal :: (ParamList p) => Bool -> p -> FunctionType p
functionTypeInternal varargs a = unsafePerformIO $ do
    let (retType:rParamTypes) = reverse (listValue a)
        paramTypes = reverse rParamTypes
    withArrayLen (map fromType paramTypes) $ \len ptr ->
        return . FunctionType . mkAny $ FFI.functionType (fromType retType) ptr
                                        (fromIntegral len) (fromBool varargs)
    
functionType :: ParamList p => p -> FunctionType p
functionType = functionTypeInternal False

functionTypeVarArg :: ParamList p => p -> FunctionType p
functionTypeVarArg = functionTypeInternal True

isFunctionVarArg :: (ParamList p) => FunctionType p -> Bool
isFunctionVarArg = toBool . FFI.isFunctionVarArg . fromType

getReturnType :: (ParamList p) => FunctionType p -> AnyType
getReturnType = mkAny . FFI.getReturnType . fromType

getParamTypes :: (ParamList p) => FunctionType p -> [AnyType]
getParamTypes typ = unsafePerformIO $ do
    let typ' = fromType typ
        count = FFI.countParamTypes typ'
        len = fromIntegral count
    allocaArray len $ \ptr -> do
      FFI.getParamTypes typ' ptr
      map mkAny <$> peekArray len ptr

newtype ArrayType a = ArrayType AnyType
    deriving (Any, Type)

arrayElement :: ArrayType a -> a
arrayElement _ = undefined

instance SequentialType (ArrayType a)

arrayType :: (Type t) => t -> ArrayType t
arrayType typ = ArrayType . mkAny $ FFI.arrayType (fromType typ) 0

instance (Type t, TypeInstance t) => TypeInstance (ArrayType t) where
    typeInstance = toAny . arrayType . typeInstance . arrayElement

newtype PointerType a = PointerType AnyType
    deriving (Any, Type)

pointerElement :: PointerType a -> a
pointerElement _ = undefined

instance SequentialType (PointerType a)

pointerType :: (Type t) => t -> PointerType t
pointerType typ = PointerType . mkAny $ FFI.pointerType (fromType typ) 0

instance (Type t, TypeInstance t) => TypeInstance (PointerType t) where
    typeInstance = toAny . pointerType . typeInstance . pointerElement

newtype VectorType a = VectorType AnyType
    deriving (Type, Any)

vectorElement :: VectorType a -> a
vectorElement _ = undefined

instance SequentialType (VectorType a)

vectorType :: (Type t) => t -> VectorType t
vectorType typ = VectorType . mkAny $ FFI.vectorType (fromType typ) 0

instance (Type t, TypeInstance t) => TypeInstance (VectorType t) where
    typeInstance = toAny . vectorType . typeInstance . vectorElement


addGlobal :: (Type t) => Module -> t -> String -> IO Value
addGlobal mod typ name =
    withModule mod $ \modPtr ->
      withCString name $ \namePtr ->
        Value <$> FFI.addGlobal modPtr (fromType typ) namePtr

setInitializer :: Value -> Value -> IO ()
setInitializer global cnst =
    FFI.setInitializer (fromValue global) (fromValue cnst)

typeOf :: Value -> AnyType
typeOf val = unsafePerformIO $ mkAny <$> FFI.typeOf (fromValue val)

addFunction :: (ParamList p) => Module -> String -> FunctionType p
            -> IO Value
addFunction mod name typ =
    withModule mod $ \modPtr ->
      withCString name $ \namePtr ->
        Value <$> FFI.addFunction modPtr namePtr (fromType typ)

deleteFunction :: Value -> IO ()
deleteFunction = FFI.deleteFunction . fromValue

maybePtr :: (Ptr a -> b) -> Ptr a -> Maybe b
maybePtr f ptr | ptr /= nullPtr = Just (f ptr)
               | otherwise = Nothing

getNamedFunction :: Module -> String -> IO (Maybe Value)
getNamedFunction mod name =
    withModule mod $ \modPtr ->
      withCString name $ \namePtr ->
        maybePtr Value <$> FFI.getNamedFunction modPtr namePtr

constWord :: (Integral a) => IntType a -> a -> Value
constWord typ val = Value $ FFI.constInt (fromType typ) (fromIntegral val) 0

constInt :: (Integral a) => IntType a -> a -> Value
constInt typ val = Value $ FFI.constInt (fromType typ) (fromIntegral val) 1

constReal :: (RealFloat a) => RealType a -> Double -> Value
constReal typ val = Value $ FFI.constReal (fromType typ) (realToFrac val)

constString :: String -> Value
constString s = unsafePerformIO $
    withCStringLen s $ \(sPtr, sLen) ->
      return . Value $ FFI.constString sPtr (fromIntegral sLen) 1

constStringNul :: String -> Value
constStringNul s = unsafePerformIO $
    withCStringLen s $ \(sPtr, sLen) ->
      return . Value $ FFI.constString sPtr (fromIntegral sLen) 0

constBitCast :: (Type t) => t -> Value -> Value
constBitCast typ val = Value $ FFI.constBitCast (fromValue val) (fromType typ)

class Const a where
    const :: a -> Value

instance Const String where
    const = constStringNul

instance Const Float where
    const = constReal floatType . fromRational . toRational

instance Const Double where
    const = constReal doubleType

instance Const Int8 where
    const = constInt int8Type . fromIntegral

instance Const Int16 where
    const = constInt int16Type . fromIntegral

instance Const Int32 where
    const = constInt int32Type . fromIntegral

instance Const Int64 where
    const = constInt int64Type

instance Const Word8 where
    const = constWord int8Type . fromIntegral

instance Const Word16 where
    const = constWord int16Type . fromIntegral

instance Const Word32 where
    const = constWord int32Type . fromIntegral

instance Const Word64 where
    const = constWord int64Type . fromIntegral


appendBasicBlock :: Value -> String -> IO BasicBlock
appendBasicBlock func name =
    withCString name $ \namePtr ->
      BasicBlock <$> FFI.appendBasicBlock (fromValue func) namePtr

insertBasicBlock :: BasicBlock -> String -> IO BasicBlock
insertBasicBlock before name =
    withCString name $ \namePtr ->
      BasicBlock <$> FFI.insertBasicBlock (fromBasicBlock before) namePtr

deleteBasicBlock :: BasicBlock -> IO ()
deleteBasicBlock = FFI.deleteBasicBlock . fromBasicBlock


withBuilder :: Builder -> (FFI.BuilderRef -> IO a) -> IO a
withBuilder bld = withForeignPtr (fromBuilder bld)

createBuilder :: IO Builder
createBuilder = do
  final <- h2c_builder FFI.disposeBuilder
  ptr <- FFI.createBuilder
  Builder <$> newForeignPtr final ptr

foreign import ccall "wrapper" h2c_builder
    :: (FFI.BuilderRef -> IO ()) -> IO (FinalizerPtr a)

positionBefore :: Builder -> Value -> IO ()
positionBefore bld insn =
    withBuilder bld $ \bldPtr ->
      FFI.positionBefore bldPtr (fromValue insn)

positionAtEnd :: Builder -> BasicBlock -> IO ()
positionAtEnd bld bblk =
    withBuilder bld $ \bldPtr ->
      FFI.positionAtEnd bldPtr (fromBasicBlock bblk)

buildGEP :: Builder -> Value -> [Value] -> String -> IO Value
buildGEP bld ptr indices name =
    withBuilder bld $ \bldPtr ->
      withCString name $ \namePtr ->
        withArrayLen (map fromValue indices) $ \idxLen idxPtr ->
          Value <$> FFI.buildGEP bldPtr (fromValue ptr) idxPtr
                                  (fromIntegral idxLen) namePtr

buildRet :: Builder -> Value -> IO Value
buildRet bld val =
    withBuilder bld $ \bldPtr ->
      Value <$> FFI.buildRet bldPtr (fromValue val)

buildCall :: Builder -> Value -> [Value] -> String -> IO Value
buildCall bld func args name =
    withBuilder bld $ \bldPtr ->
      withArrayLen (map fromValue args) $ \argLen argPtr ->
        withCString name $ \namePtr ->
          Value <$> FFI.buildCall bldPtr (fromValue func) argPtr
                                   (fromIntegral argLen) namePtr
