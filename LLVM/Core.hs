{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module LLVM.Core
    (
    -- * Modules
      createModule

    -- * Module providers
    , createModuleProviderForExistingModule

    -- * Types
    , T.mkAnyType
    , T.fromAnyType
    , T.toAnyType
    , addTypeName
    , deleteTypeName
    , getElementType

    -- ** Integer types
    , int1Type
    , int8Type
    , int16Type
    , int32Type
    , int64Type
    , integerType

    -- ** Real types
    , floatType
    , doubleType
    , x86Float80Type
    , float128Type
    , ppcFloat128Type

    -- ** Function types
    , functionType
    , functionTypeVarArg
    , isFunctionVarArg
    , getReturnType
    , getParamTypes

    -- ** Array, pointer, and vector types
    , arrayType
    , pointerType
    , vectorType

    -- ** Other types
    , voidType

    -- * Values
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
    , appendBasicBlock
    , insertBasicBlock
    , deleteBasicBlock

    -- * Instruction building
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
import LLVM.Core.Types ((:->))
import qualified LLVM.Core.Types as T
import qualified LLVM.Core.Values as V
import LLVM.Core.Instances ()


createModule :: String -> IO T.Module
createModule name =
    withCString name $ \namePtr -> do
      ptr <- FFI.moduleCreateWithName namePtr
      final <- h2c_module FFI.disposeModule
      T.Module <$> newForeignPtr final ptr

foreign import ccall "wrapper" h2c_module
    :: (FFI.ModuleRef -> IO ()) -> IO (FinalizerPtr a)


createModuleProviderForExistingModule :: T.Module -> IO T.ModuleProvider
createModuleProviderForExistingModule mod =
    T.withModule mod $ \modPtr -> do
        ptr <- FFI.createModuleProviderForExistingModule modPtr
        final <- h2c_moduleProvider FFI.disposeModuleProvider
        T.ModuleProvider <$> newForeignPtr final ptr

foreign import ccall "wrapper" h2c_moduleProvider
    :: (FFI.ModuleProviderRef -> IO ()) -> IO (FinalizerPtr a)


addTypeName :: (T.Type t) => T.Module -> t -> String -> IO Bool
addTypeName mod typ name =
    T.withModule mod $ \modPtr ->
      withCString name $ \namePtr ->
        toBool <$> FFI.addTypeName modPtr namePtr (T.fromType typ)
                 
deleteTypeName :: T.Module -> String -> IO ()
deleteTypeName mod name =
    T.withModule mod $ \modPtr ->
      withCString name $ FFI.deleteTypeName modPtr

getElementType :: T.Sequence t => t -> T.AnyType
getElementType = T.mkAnyType . FFI.getElementType . T.fromType

class TypeInstance a where
    typeInstance :: a -> T.AnyType

int1Type :: T.Int1
int1Type = T.Int1 $ T.mkAnyType FFI.int1Type

instance T.Params T.Int1 where
    listValue a = [typeInstance a]

instance TypeInstance T.Int1 where
    typeInstance _ = T.toAnyType int1Type

int8Type :: T.Int8
int8Type = T.Int8 $ T.mkAnyType FFI.int8Type

instance T.Params T.Int8 where
    listValue a = [typeInstance a]

instance TypeInstance T.Int8 where
    typeInstance _ = T.toAnyType int8Type

int16Type :: T.Int16
int16Type = T.Int16 $ T.mkAnyType FFI.int16Type

instance T.Params T.Int16 where
    listValue a = [typeInstance a]

instance TypeInstance T.Int16 where
    typeInstance _ = T.toAnyType int16Type

int32Type :: T.Int32
int32Type = T.Int32 $ T.mkAnyType FFI.int32Type

instance T.Params T.Int32 where
    listValue a = [typeInstance a]

instance TypeInstance T.Int32 where
    typeInstance _ = T.toAnyType int32Type

int64Type :: T.Int64
int64Type = T.Int64 $ T.mkAnyType FFI.int64Type

instance T.Params T.Int64 where
    listValue a = [typeInstance a]

instance TypeInstance T.Int64 where
    typeInstance _ = T.toAnyType int64Type

integerType :: Int -> T.IntWidth a
integerType = T.IntWidth . T.mkAnyType . FFI.integerType . fromIntegral

-- Not possible:
--
-- instance T.Params (T.IntWidth a) where
--     listValue a = [typeInstance a]
--
-- instance TypeInstance (T.IntWidth a) where
--     typeInstance _ = T.toAnyType integerType

floatType :: T.Float
floatType = T.Float $ T.mkAnyType FFI.floatType

instance T.Params T.Float where
    listValue a = [typeInstance a]

instance TypeInstance T.Float where
    typeInstance _ = T.toAnyType floatType

doubleType :: T.Double
doubleType = T.Double $ T.mkAnyType FFI.doubleType

instance T.Params T.Double where
    listValue a = [typeInstance a]

instance TypeInstance T.Double where
    typeInstance _ = T.toAnyType doubleType

x86Float80Type :: T.X86Float80
x86Float80Type = T.X86Float80 $ T.mkAnyType FFI.x86FP80Type

instance T.Params T.X86Float80 where
    listValue a = [typeInstance a]

instance TypeInstance T.X86Float80 where
    typeInstance _ = T.toAnyType x86Float80Type

float128Type :: T.Float128
float128Type = T.Float128 $ T.mkAnyType FFI.fp128Type

instance T.Params T.Float128 where
    listValue a = [typeInstance a]

instance TypeInstance T.Float128 where
    typeInstance _ = T.toAnyType float128Type

ppcFloat128Type :: T.PPCFloat128
ppcFloat128Type = T.PPCFloat128 $ T.mkAnyType FFI.ppcFP128Type

instance T.Params T.PPCFloat128 where
    listValue a = [typeInstance a]

instance TypeInstance T.PPCFloat128 where
    typeInstance _ = T.toAnyType ppcFloat128Type

voidType :: T.Void
voidType = T.Void $ T.mkAnyType FFI.voidType

instance T.Params T.Void where
    listValue a = [typeInstance a]

instance TypeInstance T.Void where
    typeInstance _ = T.toAnyType voidType

instance (TypeInstance a, T.Params b) => T.Params (a :-> b) where
    listValue a = typeInstance (T.car a) : T.listValue (T.cdr a)

functionTypeInternal :: (T.Params p) => Bool -> p -> T.Function p
functionTypeInternal varargs a = unsafePerformIO $ do
    let (retType:rParamTypes) = reverse (T.listValue a)
        paramTypes = reverse rParamTypes
    withArrayLen (map T.fromType paramTypes) $ \len ptr ->
        return . T.Function . T.mkAnyType $ FFI.functionType (T.fromType retType) ptr
                                        (fromIntegral len) (fromBool varargs)
    
functionType :: T.Params p => p -> T.Function p
functionType = functionTypeInternal False

instance TypeInstance p => T.Params (T.Function p) where
    listValue a = [typeInstance (T.functionParams a)]
    
functionTypeVarArg :: T.Params p => p -> T.Function p
functionTypeVarArg = functionTypeInternal True

isFunctionVarArg :: (T.Params p) => T.Function p -> Bool
isFunctionVarArg = toBool . FFI.isFunctionVarArg . T.fromType

getReturnType :: (T.Params p) => T.Function p -> T.AnyType
getReturnType = T.mkAnyType . FFI.getReturnType . T.fromType

getParamTypes :: (T.Params p) => T.Function p -> [T.AnyType]
getParamTypes typ = unsafePerformIO $ do
    let typ' = T.fromType typ
        count = FFI.countParamTypes typ'
        len = fromIntegral count
    allocaArray len $ \ptr -> do
      FFI.getParamTypes typ' ptr
      map T.mkAnyType <$> peekArray len ptr

arrayType :: (T.Type t) => t -> T.Array t
arrayType typ = T.Array . T.mkAnyType $ FFI.arrayType (T.fromType typ) 0

instance V.TypedValue V.AnyValue T.AnyType where
    valueType a = T.mkAnyType (typeOf a)

instance (T.Type t, TypeInstance t) => TypeInstance (T.Array t) where
    typeInstance = T.toAnyType . arrayType . typeInstance . T.arrayElement

pointerType :: (T.Type t) => t -> T.Pointer t
pointerType typ = T.Pointer . T.mkAnyType $ FFI.pointerType (T.fromType typ) 0

instance (T.Type t, TypeInstance t) => TypeInstance (T.Pointer t) where
    typeInstance = T.toAnyType . pointerType . typeInstance . T.pointerElement

vectorType :: (T.Type t) => t -> T.Vector t
vectorType typ = T.Vector . T.mkAnyType $ FFI.vectorType (T.fromType typ) 0

instance (T.Type t, TypeInstance t) => TypeInstance (T.Vector t) where
    typeInstance = T.toAnyType . vectorType . typeInstance . T.vectorElement


addGlobal :: (T.Type t) => T.Module -> t -> String -> IO (V.GlobalVar t)
addGlobal mod typ name =
    T.withModule mod $ \modPtr ->
      withCString name $ \namePtr ->
        V.GlobalVar . V.mkAnyValue <$> FFI.addGlobal modPtr (T.fromType typ) namePtr

setInitializer :: V.ConstValue t => V.GlobalVar a -> t -> IO ()
setInitializer global cnst =
    FFI.setInitializer (V.fromValue global) (V.fromValue cnst)

typeOf :: V.Value t => t -> T.AnyType
typeOf val = unsafePerformIO $ T.mkAnyType <$> FFI.typeOf (V.fromValue val)

addFunction :: (T.Params p) => T.Module -> String -> T.Function p
            -> IO V.Function
addFunction mod name typ =
    T.withModule mod $ \modPtr ->
      withCString name $ \namePtr ->
        V.Function . V.mkAnyValue <$> FFI.addFunction modPtr namePtr (T.fromType typ)

deleteFunction :: V.Function -> IO ()
deleteFunction = FFI.deleteFunction . V.fromValue

maybePtr :: (Ptr a -> b) -> Ptr a -> Maybe b
maybePtr f ptr | ptr /= nullPtr = Just (f ptr)
               | otherwise = Nothing

getNamedFunction :: T.Module -> String -> IO (Maybe V.Function)
getNamedFunction mod name =
    T.withModule mod $ \modPtr ->
      withCString name $ \namePtr ->
        maybePtr (V.Function . V.mkAnyValue) <$> FFI.getNamedFunction modPtr namePtr

constWord :: (T.Integer t, Integral a) => t -> a -> V.ConstInt t
constWord typ val =
    V.ConstInt . V.mkAnyValue $ FFI.constInt (T.fromType typ) (fromIntegral val) 0

constInt :: (T.Integer t, Integral a) => t -> a -> V.ConstInt t
constInt typ val =
    V.ConstInt . V.mkAnyValue $ FFI.constInt (T.fromType typ) (fromIntegral val) 1

constReal :: (T.Real t, RealFloat a) => t -> a -> V.ConstReal t
constReal typ val = V.ConstReal . V.mkAnyValue $ FFI.constReal (T.fromType typ) (realToFrac val)

att :: V.ConstArray a -> T.Pointer a
att = T.fromAnyType . T.mkAnyType . typeOf

instance V.TypedValue (V.ConstArray a) (T.Pointer a) where
    valueType = att

constString :: String -> V.ConstArray T.Int8
constString s = unsafePerformIO $
    withCStringLen s $ \(sPtr, sLen) ->
      return . V.ConstArray . V.mkAnyValue $ FFI.constString sPtr (fromIntegral sLen) 1

constStringNul :: String -> V.ConstArray T.Int8
constStringNul s = unsafePerformIO $
    withCStringLen s $ \(sPtr, sLen) ->
      return . V.ConstArray . V.mkAnyValue $ FFI.constString sPtr (fromIntegral sLen) 0

constBitCast :: (V.ConstValue a, V.ConstValue b, V.HasAnyValue b, T.Type t) => t -> a -> b
constBitCast typ val =
    V.fromAnyValue . V.mkAnyValue $ FFI.constBitCast (V.fromValue val) (T.fromType typ)

class V.ConstValue t => Const a t | a -> t where
    const :: a -> t

instance Const String (V.ConstArray T.Int8) where
    const = constStringNul

instance Const Float (V.ConstReal T.Float) where
    const = constReal floatType . fromRational . toRational

instance Const Double (V.ConstReal T.Double) where
    const = constReal doubleType

instance Const Int8 (V.ConstInt T.Int8) where
    const = constInt int8Type . fromIntegral

instance Const Int16 (V.ConstInt T.Int16) where
    const = constInt int16Type . fromIntegral

instance Const Int32 (V.ConstInt T.Int32) where
    const = constInt int32Type . fromIntegral

instance Const Int64 (V.ConstInt T.Int64) where
    const = constInt int64Type

instance Const Word8 (V.ConstInt T.Int8) where
    const = constWord int8Type . fromIntegral

instance Const Word16 (V.ConstInt T.Int16) where
    const = constWord int16Type . fromIntegral

instance Const Word32 (V.ConstInt T.Int32) where
    const = constWord int32Type . fromIntegral

instance Const Word64 (V.ConstInt T.Int64) where
    const = constWord int64Type . fromIntegral


appendBasicBlock :: V.Function -> String -> IO V.BasicBlock
appendBasicBlock func name =
    withCString name $ \namePtr ->
      V.BasicBlock . V.mkAnyValue <$> FFI.appendBasicBlock (V.fromValue func) namePtr

insertBasicBlock :: V.BasicBlock -> String -> IO V.BasicBlock
insertBasicBlock before name =
    withCString name $ \namePtr ->
      V.BasicBlock . V.mkAnyValue <$> FFI.insertBasicBlock (V.fromValue before) namePtr

deleteBasicBlock :: V.BasicBlock -> IO ()
deleteBasicBlock = FFI.deleteBasicBlock . V.fromValue


withBuilder :: V.Builder -> (FFI.BuilderRef -> IO a) -> IO a
withBuilder = withForeignPtr . V.fromBuilder

createBuilder :: IO V.Builder
createBuilder = do
  final <- h2c_builder FFI.disposeBuilder
  ptr <- FFI.createBuilder
  V.Builder <$> newForeignPtr final ptr

foreign import ccall "wrapper" h2c_builder
    :: (FFI.BuilderRef -> IO ()) -> IO (FinalizerPtr a)

positionBefore :: V.Instruction i => V.Builder -> i -> IO ()
positionBefore bld insn =
    withBuilder bld $ \bldPtr ->
      FFI.positionBefore bldPtr (V.fromValue insn)

positionAtEnd :: V.Builder -> V.BasicBlock -> IO ()
positionAtEnd bld bblk =
    withBuilder bld $ \bldPtr ->
      FFI.positionAtEnd bldPtr (V.fromValue bblk)

buildGEP :: (V.Value p, V.Value i) => V.Builder -> p -> [i] -> String
         -> IO V.GetElementPtrInst
buildGEP bld ptr indices name =
    withBuilder bld $ \bldPtr ->
      withCString name $ \namePtr ->
        withArrayLen (map V.fromValue indices) $ \idxLen idxPtr ->
          V.GetElementPtrInst . V.mkAnyValue <$> FFI.buildGEP bldPtr (V.fromValue ptr) idxPtr
                                  (fromIntegral idxLen) namePtr

buildRet :: V.Value a => V.Builder -> a -> IO V.ReturnInst
buildRet bld val =
    withBuilder bld $ \bldPtr ->
      V.ReturnInst . V.mkAnyValue <$> FFI.buildRet bldPtr (V.fromValue val)

buildCall :: V.Builder -> V.Function -> [V.AnyValue] -> String
          -> IO V.CallInst
buildCall bld func args name =
    withBuilder bld $ \bldPtr ->
      withArrayLen (map V.fromValue args) $ \argLen argPtr ->
        withCString name $ \namePtr ->
          V.CallInst . V.mkAnyValue <$> FFI.buildCall bldPtr (V.fromValue func) argPtr
                                   (fromIntegral argLen) namePtr
