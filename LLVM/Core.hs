module LLVM.Core
    (
    -- * Modules
      createModule

    -- * Module providers
    , createModuleProviderForExistingModule

    -- * Types
    , T.mkAny
    , T.fromAny
    , T.toAny
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
    , ParamList(..)
    , Return
    , (:->)
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
import qualified LLVM.Core.Types as T
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
getElementType = T.mkAny . FFI.getElementType . T.fromType

class TypeInstance a where
    typeInstance :: a -> T.AnyType

int1Type :: T.Int1
int1Type = T.Int1 $ T.mkAny FFI.int1Type

instance TypeInstance T.Int1 where
    typeInstance _ = T.toAny int1Type

int8Type :: T.Int8
int8Type = T.Int8 $ T.mkAny FFI.int8Type

instance TypeInstance T.Int8 where
    typeInstance _ = T.toAny int8Type

int16Type :: T.Int16
int16Type = T.Int16 $ T.mkAny FFI.int16Type

instance TypeInstance T.Int16 where
    typeInstance _ = T.toAny int16Type

int32Type :: T.Int32
int32Type = T.Int32 $ T.mkAny FFI.int32Type

instance TypeInstance T.Int32 where
    typeInstance _ = T.toAny int32Type

int64Type :: T.Int64
int64Type = T.Int64 $ T.mkAny FFI.int64Type

instance TypeInstance T.Int64 where
    typeInstance _ = T.toAny int64Type

integerType :: Int -> T.IntWidth a
integerType = T.IntWidth . T.mkAny . FFI.integerType . fromIntegral

floatType :: T.Float
floatType = T.Float $ T.mkAny FFI.floatType

instance TypeInstance T.Float where
    typeInstance _ = T.toAny floatType

doubleType :: T.Double
doubleType = T.Double $ T.mkAny FFI.doubleType

instance TypeInstance T.Double where
    typeInstance _ = T.toAny doubleType

x86Float80Type :: T.X86Float80
x86Float80Type = T.X86Float80 $ T.mkAny FFI.x86FP80Type

instance TypeInstance T.X86Float80 where
    typeInstance _ = T.toAny x86Float80Type

float128Type :: T.Float128
float128Type = T.Float128 $ T.mkAny FFI.fp128Type

instance TypeInstance T.Float128 where
    typeInstance _ = T.toAny float128Type

ppcFloat128Type :: T.PPCFloat128
ppcFloat128Type = T.PPCFloat128 $ T.mkAny FFI.ppcFP128Type

instance TypeInstance T.PPCFloat128 where
    typeInstance _ = T.toAny ppcFloat128Type

voidType :: T.Void
voidType = T.Void $ T.mkAny FFI.voidType

instance TypeInstance T.Void where
    typeInstance _ = T.toAny voidType

data a :-> b
infixr 6 :->

car :: (a :-> b) -> a
car _ = undefined

cdr :: (a :-> b) -> b
cdr _ = undefined

class ParamList l where
    listValue :: l -> [T.AnyType]

data Return a

ret :: Return a -> a
ret _ = undefined

instance TypeInstance a => ParamList (Return a) where
    listValue a = [typeInstance (ret a)]

instance (TypeInstance a, ParamList b) => ParamList (a :-> b) where
    listValue a = typeInstance (car a) : listValue (cdr a)

functionTypeInternal :: (ParamList p) => Bool -> p -> T.Function p
functionTypeInternal varargs a = unsafePerformIO $ do
    let (retType:rParamTypes) = reverse (listValue a)
        paramTypes = reverse rParamTypes
    withArrayLen (map T.fromType paramTypes) $ \len ptr ->
        return . T.Function . T.mkAny $ FFI.functionType (T.fromType retType) ptr
                                        (fromIntegral len) (fromBool varargs)
    
functionType :: ParamList p => p -> T.Function p
functionType = functionTypeInternal False

functionTypeVarArg :: ParamList p => p -> T.Function p
functionTypeVarArg = functionTypeInternal True

isFunctionVarArg :: (ParamList p) => T.Function p -> Bool
isFunctionVarArg = toBool . FFI.isFunctionVarArg . T.fromType

getReturnType :: (ParamList p) => T.Function p -> T.AnyType
getReturnType = T.mkAny . FFI.getReturnType . T.fromType

getParamTypes :: (ParamList p) => T.Function p -> [T.AnyType]
getParamTypes typ = unsafePerformIO $ do
    let typ' = T.fromType typ
        count = FFI.countParamTypes typ'
        len = fromIntegral count
    allocaArray len $ \ptr -> do
      FFI.getParamTypes typ' ptr
      map T.mkAny <$> peekArray len ptr

arrayElement :: T.Array a -> a
arrayElement _ = undefined

arrayType :: (T.Type t) => t -> T.Array t
arrayType typ = T.Array . T.mkAny $ FFI.arrayType (T.fromType typ) 0

instance (T.Type t, TypeInstance t) => TypeInstance (T.Array t) where
    typeInstance = T.toAny . arrayType . typeInstance . arrayElement

pointerElement :: T.Pointer a -> a
pointerElement _ = undefined

pointerType :: (T.Type t) => t -> T.Pointer t
pointerType typ = T.Pointer . T.mkAny $ FFI.pointerType (T.fromType typ) 0

instance (T.Type t, TypeInstance t) => TypeInstance (T.Pointer t) where
    typeInstance = T.toAny . pointerType . typeInstance . pointerElement

vectorElement :: T.Vector a -> a
vectorElement _ = undefined

vectorType :: (T.Type t) => t -> T.Vector t
vectorType typ = T.Vector . T.mkAny $ FFI.vectorType (T.fromType typ) 0

instance (T.Type t, TypeInstance t) => TypeInstance (T.Vector t) where
    typeInstance = T.toAny . vectorType . typeInstance . vectorElement


addGlobal :: (T.Type t) => T.Module -> t -> String -> IO T.Value
addGlobal mod typ name =
    T.withModule mod $ \modPtr ->
      withCString name $ \namePtr ->
        T.Value <$> FFI.addGlobal modPtr (T.fromType typ) namePtr

setInitializer :: T.Value -> T.Value -> IO ()
setInitializer global cnst =
    FFI.setInitializer (T.fromValue global) (T.fromValue cnst)

typeOf :: T.Value -> T.AnyType
typeOf val = unsafePerformIO $ T.mkAny <$> FFI.typeOf (T.fromValue val)

addFunction :: (ParamList p) => T.Module -> String -> T.Function p
            -> IO T.Value
addFunction mod name typ =
    T.withModule mod $ \modPtr ->
      withCString name $ \namePtr ->
        T.Value <$> FFI.addFunction modPtr namePtr (T.fromType typ)

deleteFunction :: T.Value -> IO ()
deleteFunction = FFI.deleteFunction . T.fromValue

maybePtr :: (Ptr a -> b) -> Ptr a -> Maybe b
maybePtr f ptr | ptr /= nullPtr = Just (f ptr)
               | otherwise = Nothing

getNamedFunction :: T.Module -> String -> IO (Maybe T.Value)
getNamedFunction mod name =
    T.withModule mod $ \modPtr ->
      withCString name $ \namePtr ->
        maybePtr T.Value <$> FFI.getNamedFunction modPtr namePtr

constWord :: (T.Integer t, Integral a) => t -> a -> T.Value
constWord typ val =
    T.Value $ FFI.constInt (T.fromType typ) (fromIntegral val) 0

constInt :: (T.Integer t, Integral a) => t -> a -> T.Value
constInt typ val =
    T.Value $ FFI.constInt (T.fromType typ) (fromIntegral val) 1

constReal :: (T.Real t, RealFloat a) => t -> a -> T.Value
constReal typ val = T.Value $ FFI.constReal (T.fromType typ) (realToFrac val)

constString :: String -> T.Value
constString s = unsafePerformIO $
    withCStringLen s $ \(sPtr, sLen) ->
      return . T.Value $ FFI.constString sPtr (fromIntegral sLen) 1

constStringNul :: String -> T.Value
constStringNul s = unsafePerformIO $
    withCStringLen s $ \(sPtr, sLen) ->
      return . T.Value $ FFI.constString sPtr (fromIntegral sLen) 0

constBitCast :: (T.Type t) => t -> T.Value -> T.Value
constBitCast typ val =
    T.Value $ FFI.constBitCast (T.fromValue val) (T.fromType typ)

class Const a where
    const :: a -> T.Value

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


appendBasicBlock :: T.Value -> String -> IO T.BasicBlock
appendBasicBlock func name =
    withCString name $ \namePtr ->
      T.BasicBlock <$> FFI.appendBasicBlock (T.fromValue func) namePtr

insertBasicBlock :: T.BasicBlock -> String -> IO T.BasicBlock
insertBasicBlock before name =
    withCString name $ \namePtr ->
      T.BasicBlock <$> FFI.insertBasicBlock (T.fromBasicBlock before) namePtr

deleteBasicBlock :: T.BasicBlock -> IO ()
deleteBasicBlock = FFI.deleteBasicBlock . T.fromBasicBlock


withBuilder :: T.Builder -> (FFI.BuilderRef -> IO a) -> IO a
withBuilder bld = withForeignPtr (T.fromBuilder bld)

createBuilder :: IO T.Builder
createBuilder = do
  final <- h2c_builder FFI.disposeBuilder
  ptr <- FFI.createBuilder
  T.Builder <$> newForeignPtr final ptr

foreign import ccall "wrapper" h2c_builder
    :: (FFI.BuilderRef -> IO ()) -> IO (FinalizerPtr a)

positionBefore :: T.Builder -> T.Value -> IO ()
positionBefore bld insn =
    withBuilder bld $ \bldPtr ->
      FFI.positionBefore bldPtr (T.fromValue insn)

positionAtEnd :: T.Builder -> T.BasicBlock -> IO ()
positionAtEnd bld bblk =
    withBuilder bld $ \bldPtr ->
      FFI.positionAtEnd bldPtr (T.fromBasicBlock bblk)

buildGEP :: T.Builder -> T.Value -> [T.Value] -> String -> IO T.Value
buildGEP bld ptr indices name =
    withBuilder bld $ \bldPtr ->
      withCString name $ \namePtr ->
        withArrayLen (map T.fromValue indices) $ \idxLen idxPtr ->
          T.Value <$> FFI.buildGEP bldPtr (T.fromValue ptr) idxPtr
                                  (fromIntegral idxLen) namePtr

buildRet :: T.Builder -> T.Value -> IO T.Value
buildRet bld val =
    withBuilder bld $ \bldPtr ->
      T.Value <$> FFI.buildRet bldPtr (T.fromValue val)

buildCall :: T.Builder -> T.Value -> [T.Value] -> String -> IO T.Value
buildCall bld func args name =
    withBuilder bld $ \bldPtr ->
      withArrayLen (map T.fromValue args) $ \argLen argPtr ->
        withCString name $ \namePtr ->
          T.Value <$> FFI.buildCall bldPtr (T.fromValue func) argPtr
                                   (fromIntegral argLen) namePtr
