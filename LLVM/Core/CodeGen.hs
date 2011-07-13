{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances, UndecidableInstances, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable, Rank2Types #-}
module LLVM.Core.CodeGen(
    -- * Module creation
    newModule, newNamedModule, defineModule, createModule,
    getModuleValues, ModuleValue, castModuleValue,
    -- * Globals
    Linkage(..),
    Visibility(..),
    -- * Function creation
    Function, newFunction, newNamedFunction, defineFunction, createFunction, createNamedFunction, setFuncCallConv,
    addAttributes,
    FFI.Attribute(..),
    externFunction, staticFunction,
    FunctionArgs, FunctionRet,
    TFunction,
    -- * Global variable creation
    Global, newGlobal, newNamedGlobal, defineGlobal, createGlobal, createNamedGlobal, TGlobal,
    externGlobal, staticGlobal,
    -- * Values
    Value(..), ConstValue(..),
    IsConst(..), valueOf, value,
    zero, allOnes, undef,
    createString, createStringNul,
    withString, withStringNul,
    constVector, constArray, constStruct, constPackedStruct,
    -- * Basic blocks
    BasicBlock(..), newBasicBlock, newNamedBasicBlock, defineBasicBlock, createBasicBlock, getCurrentBasicBlock,
    fromLabel, toLabel,
    -- * Misc
    withCurrentBuilder
    ) where
import Data.Typeable
import Control.Monad(liftM, when)
import Data.Int
import Data.Word
import Foreign.StablePtr (StablePtr, castStablePtrToPtr)
import Foreign.Ptr(minusPtr, nullPtr, castPtr, FunPtr, castFunPtrToPtr)
import Foreign.Storable(sizeOf)
import Data.TypeLevel hiding (Bool, Eq, (+), (==))
import LLVM.Core.CodeGenMonad
import qualified LLVM.FFI.Core as FFI
import LLVM.FFI.Core(Linkage(..), Visibility(..))
import qualified LLVM.Core.Util as U
import LLVM.Core.Type
import LLVM.Core.Data

--------------------------------------

-- | Create a new module.
newModule :: IO U.Module
newModule = newNamedModule "_module"  -- XXX should generate a name

-- | Create a new explicitely named module.
newNamedModule :: String              -- ^ module name
               -> IO U.Module
newNamedModule = U.createModule

-- | Give the body for a module.
defineModule :: U.Module              -- ^ module that is defined
             -> CodeGenModule a       -- ^ module body
             -> IO a
defineModule = runCodeGenModule

-- | Create a new module with the given body.
createModule :: CodeGenModule a       -- ^ module body
             -> IO a
createModule cgm = newModule >>= \ m -> defineModule m cgm

--------------------------------------

newtype ModuleValue = ModuleValue FFI.ValueRef
    deriving (Show, Typeable)

getModuleValues :: U.Module -> IO [(String, ModuleValue)]
getModuleValues = liftM (map (\ (s,p) -> (s, ModuleValue p))) . U.getModuleValues

castModuleValue :: forall a . (IsType a) => ModuleValue -> Maybe (Value a)
castModuleValue (ModuleValue f) =
    if U.valueHasType f (typeRef (undefined :: a)) then Just (Value f) else Nothing

--------------------------------------

newtype Value a = Value { unValue :: FFI.ValueRef }
    deriving (Show, Typeable)

newtype ConstValue a = ConstValue { unConstValue :: FFI.ValueRef }
    deriving (Show, Typeable)

-- XXX merge with IsArithmetic?
class IsConst a where
    constOf :: a -> ConstValue a

instance IsConst Bool   where constOf = constEnum (typeRef True)
--instance IsConst Char   where constOf = constEnum (typeRef (0::Word8)) -- XXX Unicode
instance IsConst Word8  where constOf = constI
instance IsConst Word16 where constOf = constI
instance IsConst Word32 where constOf = constI
instance IsConst Word64 where constOf = constI
instance IsConst Int8   where constOf = constI
instance IsConst Int16  where constOf = constI
instance IsConst Int32  where constOf = constI
instance IsConst Int64  where constOf = constI
instance IsConst Float  where constOf = constF
instance IsConst Double where constOf = constF
--instance IsConst FP128  where constOf = constF

constOfPtr :: (IsType a) =>
    a -> Ptr b -> ConstValue a
constOfPtr proto p =
    let ip = p `minusPtr` nullPtr
        inttoptrC (ConstValue v) = ConstValue $ FFI.constIntToPtr v (typeRef proto)
    in  if sizeOf p == 4 then
            inttoptrC $ constOf (fromIntegral ip :: Word32)
        else if sizeOf p == 8 then
            inttoptrC $ constOf (fromIntegral ip :: Word64)
        else
            error "constOf Ptr: pointer size not 4 or 8"

-- This instance doesn't belong here, but mutually recursive modules are painful.
instance (IsType a) => IsConst (Ptr a) where
    constOf p = constOfPtr p p

instance IsConst (StablePtr a) where
    constOf p = constOfPtr p (castStablePtrToPtr p)

instance (IsPrimitive a, IsConst a, Pos n) => IsConst (Vector n a) where
    constOf (Vector xs) = constVector (map constOf xs)

instance (IsConst a, IsSized a s, Nat n) => IsConst (Array n a) where
    constOf (Array xs) = constArray (map constOf xs)

instance (IsConstFields a) => IsConst (Struct a) where
    constOf (Struct a) = ConstValue $ U.constStruct (constFieldsOf a) False
instance (IsConstFields a) => IsConst (PackedStruct a) where
    constOf (PackedStruct a) = ConstValue $ U.constStruct (constFieldsOf a) True

class IsConstFields a where
    constFieldsOf :: a -> [FFI.ValueRef]

instance (IsConst a, IsConstFields as) => IsConstFields (a, as) where
    constFieldsOf (a, as) = unConstValue (constOf a) : constFieldsOf as
instance IsConstFields () where
    constFieldsOf _ = []

constEnum :: (Enum a) => FFI.TypeRef -> a -> ConstValue a
constEnum t i = ConstValue $ FFI.constInt t (fromIntegral $ fromEnum i) 0

constI :: (IsInteger a, Integral a) => a -> ConstValue a
constI i = ConstValue $ FFI.constInt (typeRef i) (fromIntegral i) (fromIntegral $ fromEnum $ isSigned i)

constF :: (IsFloating a, Real a) => a -> ConstValue a
constF i = ConstValue $ FFI.constReal (typeRef i) (realToFrac i)

valueOf :: (IsConst a) => a -> Value a
valueOf = value . constOf

value :: ConstValue a -> Value a
value (ConstValue a) = Value a

zero :: forall a . (IsType a) => ConstValue a
zero = ConstValue $ FFI.constNull $ typeRef (undefined :: a)

allOnes :: forall a . (IsInteger a) => ConstValue a
allOnes = ConstValue $ FFI.constAllOnes $ typeRef (undefined :: a)

undef :: forall a . (IsType a) => ConstValue a
undef = ConstValue $ FFI.getUndef $ typeRef (undefined :: a)

{-
createString :: String -> ConstValue (DynamicArray Word8)
createString = ConstValue . U.constString

constStringNul :: String -> ConstValue (DynamicArray Word8)
constStringNul = ConstValue . U.constStringNul
-}

--------------------------------------

type FunctionRef = FFI.ValueRef

-- |A function is simply a pointer to the function.
type Function a = Value (Ptr a)

-- | Create a new named function.
newNamedFunction :: forall a . (IsFunction a)
                 => Linkage
                 -> String   -- ^ Function name
                 -> CodeGenModule (Function a)
newNamedFunction linkage name = do
    modul <- getModule
    let typ = typeRef (undefined :: a)
    liftIO $ liftM Value $ U.addFunction modul linkage name typ

-- | Create a new function.  Use 'newNamedFunction' to create a function with external linkage, since
-- it needs a known name.
newFunction :: forall a . (IsFunction a)
            => Linkage
            -> CodeGenModule (Function a)
newFunction linkage = genMSym "fun" >>= newNamedFunction linkage

-- | Define a function body.  The basic block returned by the function is the function entry point.
defineFunction :: forall f g r . (FunctionArgs f g r)
               => Function f       -- ^ Function to define (created by 'newFunction').
               -> g                -- ^ Function body.
               -> CodeGenModule ()
defineFunction (Value fn) body = do
    bld <- liftIO $ U.createBuilder
    let body' = do
	    l <- newBasicBlock
	    defineBasicBlock l
	    applyArgs fn body :: CodeGenFunction r ()
    runCodeGenFunction bld fn body'
    return ()

-- | Create a new function with the given body.
createFunction :: (IsFunction f, FunctionArgs f g r)
               => Linkage
               -> g  -- ^ Function body.
               -> CodeGenModule (Function f)
createFunction linkage body = do
    f <- newFunction linkage
    defineFunction f body
    return f

-- | Create a new function with the given body.
createNamedFunction :: (IsFunction f, FunctionArgs f g r)
               => Linkage
	       -> String
               -> g  -- ^ Function body.
               -> CodeGenModule (Function f)
createNamedFunction linkage name body = do
    f <- newNamedFunction linkage name
    defineFunction f body
    return f

-- | Set the calling convention of a function. By default it is the
-- C calling convention.
setFuncCallConv :: Function a
                -> FFI.CallingConvention
                -> CodeGenModule ()
setFuncCallConv (Value f) cc = do
  liftIO $ FFI.setFunctionCallConv f (FFI.fromCallingConvention cc)
  return ()

-- | Add attributes to a value.  Beware, what attributes are allowed depends on
-- what kind of value it is.
addAttributes :: Value a -> Int -> [FFI.Attribute] -> CodeGenFunction r ()
addAttributes (Value f) i as = do
    liftIO $ FFI.addInstrAttribute f (fromIntegral i) (sum $ map FFI.fromAttribute as)

-- Convert a function of type f = t1->t2->...-> IO r to
-- g = Value t1 -> Value t2 -> ... CodeGenFunction r ()
class FunctionArgs f g r | f -> g r, g r -> f where
    apArgs :: Int -> FunctionRef -> g -> FA r

applyArgs :: (FunctionArgs f g r) => FunctionRef -> g -> FA r
applyArgs = apArgs 0

instance (FunctionArgs b b' r) => FunctionArgs (a -> b) (Value a -> b') r where
    apArgs n f g = apArgs (n+1) f (g $ Value $ U.getParam f n)

-- XXX instances for all IsFirstClass functions,
-- because Haskell can't deal with the context and the FD
type FA a = CodeGenFunction a ()
instance FunctionArgs (IO Float)         (FA Float)         Float         where apArgs _ _ g = g
instance FunctionArgs (IO Double)        (FA Double)        Double        where apArgs _ _ g = g
instance FunctionArgs (IO FP128)         (FA FP128)         FP128         where apArgs _ _ g = g
instance (Pos n) => 
         FunctionArgs (IO (IntN n))      (FA (IntN n))      (IntN n)      where apArgs _ _ g = g
instance (Pos n) =>
         FunctionArgs (IO (WordN n))     (FA (WordN n))     (WordN n)     where apArgs _ _ g = g
instance FunctionArgs (IO Bool)          (FA Bool)          Bool          where apArgs _ _ g = g
instance FunctionArgs (IO Int8)          (FA Int8)          Int8          where apArgs _ _ g = g
instance FunctionArgs (IO Int16)         (FA Int16)         Int16         where apArgs _ _ g = g
instance FunctionArgs (IO Int32)         (FA Int32)         Int32         where apArgs _ _ g = g
instance FunctionArgs (IO Int64)         (FA Int64)         Int64         where apArgs _ _ g = g
instance FunctionArgs (IO Word8)         (FA Word8)         Word8         where apArgs _ _ g = g
instance FunctionArgs (IO Word16)        (FA Word16)        Word16        where apArgs _ _ g = g
instance FunctionArgs (IO Word32)        (FA Word32)        Word32        where apArgs _ _ g = g
instance FunctionArgs (IO Word64)        (FA Word64)        Word64        where apArgs _ _ g = g
instance FunctionArgs (IO ())            (FA ())            ()            where apArgs _ _ g = g
instance (Pos n, IsPrimitive a) =>
         FunctionArgs (IO (Vector n a))  (FA (Vector n a))  (Vector n a)  where apArgs _ _ g = g
instance (IsType a) => 
         FunctionArgs (IO (Ptr a))       (FA (Ptr a))       (Ptr a)       where apArgs _ _ g = g
instance FunctionArgs (IO (StablePtr a)) (FA (StablePtr a)) (StablePtr a) where apArgs _ _ g = g

-- |This class is just to simplify contexts.
class (FunctionArgs (IO a) (CodeGenFunction a ()) a) => FunctionRet a
instance (FunctionArgs (IO a) (CodeGenFunction a ()) a) => FunctionRet a

--------------------------------------

-- |A basic block is a sequence of non-branching instructions, terminated by a control flow instruction.
newtype BasicBlock = BasicBlock FFI.BasicBlockRef
    deriving (Show, Typeable)

createBasicBlock :: CodeGenFunction r BasicBlock
createBasicBlock = do
    b <- newBasicBlock
    defineBasicBlock b
    return b

newBasicBlock :: CodeGenFunction r BasicBlock
newBasicBlock = genFSym >>= newNamedBasicBlock

newNamedBasicBlock :: String -> CodeGenFunction r BasicBlock
newNamedBasicBlock name = do
    fn <- getFunction
    liftIO $ liftM BasicBlock $ U.appendBasicBlock fn name

defineBasicBlock :: BasicBlock -> CodeGenFunction r ()
defineBasicBlock (BasicBlock l) = do
    bld <- getBuilder
    liftIO $ U.positionAtEnd bld l

getCurrentBasicBlock :: CodeGenFunction r BasicBlock
getCurrentBasicBlock = do
    bld <- getBuilder
    liftIO $ liftM BasicBlock $ U.getInsertBlock bld

toLabel :: BasicBlock -> Value Label
toLabel (BasicBlock ptr) = Value (FFI.basicBlockAsValue ptr)

fromLabel :: Value Label -> BasicBlock
fromLabel (Value ptr) = BasicBlock (FFI.valueAsBasicBlock ptr)

--------------------------------------

--- XXX: the functions in this section (and addGlobalMapping) don't actually use any
-- Function state so should really be in the CodeGenModule monad

-- | Create a reference to an external function while code generating for a function.
-- If LLVM cannot resolve its name, then you may try 'staticFunction'.
externFunction :: forall a r . (IsFunction a) => String -> CodeGenFunction r (Function a)
externFunction name = externCore name $ fmap (unValue :: Function a -> FFI.ValueRef) . newNamedFunction ExternalLinkage

-- | As 'externFunction', but for 'Global's rather than 'Function's
externGlobal :: forall a r . (IsType a) => Bool -> String -> CodeGenFunction r (Global a)
externGlobal isConst name = externCore name $ fmap (unValue :: Global a -> FFI.ValueRef) . newNamedGlobal isConst ExternalLinkage

externCore :: forall a r . String -> (String -> CodeGenModule FFI.ValueRef) -> CodeGenFunction r (Global a)
externCore name act = do
    es <- getExterns
    case lookup name es of
        Just f -> return $ Value f
        Nothing -> do
            f <- liftCodeGenModule $ act name
            putExterns ((name, f) : es)
	    return $ Value f

{- |
Make an external C function with a fixed address callable from LLVM code.
This callback function can also be a Haskell function,
that was imported like

> foreign import ccall "&nextElement"
>    nextElementFunPtr :: FunPtr (StablePtr (IORef [Word32]) -> IO Word32)

See @examples\/List.hs@.

When you only use 'externFunction', then LLVM cannot resolve the name.
(However, I do not know why.)
Thus 'staticFunction' manages a list of static functions.
This list is automatically installed by 'ExecutionEngine.simpleFunction'
and can be manually obtained by 'getGlobalMappings'
and installed by 'ExecutionEngine.addGlobalMappings'.
\"Installing\" means calling LLVM's @addGlobalMapping@ according to
<http://old.nabble.com/jit-with-external-functions-td7769793.html>.
-}
staticFunction :: forall f r. (IsFunction f) => FunPtr f -> CodeGenFunction r (Function f)
staticFunction func = liftCodeGenModule $ do
    val <- newNamedFunction ExternalLinkage ""
    addGlobalMapping (unValue (val :: Function f)) (castFunPtrToPtr func)
    return val

-- | As 'staticFunction', but for 'Global's rather than 'Function's
staticGlobal :: forall a r. (IsType a) => Bool -> Ptr a -> CodeGenFunction r (Global a)
staticGlobal isConst gbl = liftCodeGenModule $ do
    val <- newNamedGlobal isConst ExternalLinkage ""
    addGlobalMapping (unValue (val :: Global a)) (castPtr gbl)
    return val

--------------------------------------

withCurrentBuilder :: (FFI.BuilderRef -> IO a) -> CodeGenFunction r a
withCurrentBuilder body = do
    bld <- getBuilder
    liftIO $ U.withBuilder bld body

--------------------------------------

-- Mark all block terminating instructions.  Not used yet.
--data Terminate = Terminate

--------------------------------------

type Global a = Value (Ptr a)

-- | Create a new named global variable.
newNamedGlobal :: forall a . (IsType a)
               => Bool         -- ^Constant?
               -> Linkage      -- ^Visibility
               -> String       -- ^Name
               -> TGlobal a
newNamedGlobal isConst linkage name = do
    modul <- getModule
    let typ = typeRef (undefined :: a)
    liftIO $ liftM Value $ do g <- U.addGlobal modul linkage name typ
    	     	   	      when isConst $ FFI.setGlobalConstant g 1
			      return g

-- | Create a new global variable.
newGlobal :: forall a . (IsType a) => Bool -> Linkage -> TGlobal a
newGlobal isConst linkage = genMSym "glb" >>= newNamedGlobal isConst linkage

-- | Give a global variable a (constant) value.
defineGlobal :: Global a -> ConstValue a -> CodeGenModule ()
defineGlobal (Value g) (ConstValue v) =
    liftIO $ FFI.setInitializer g v

-- | Create and define a global variable.
createGlobal :: (IsType a) => Bool -> Linkage -> ConstValue a -> TGlobal a
createGlobal isConst linkage con = do
    g <- newGlobal isConst linkage
    defineGlobal g con
    return g

-- | Create and define a named global variable.
createNamedGlobal :: (IsType a) => Bool -> Linkage -> String -> ConstValue a -> TGlobal a
createNamedGlobal isConst linkage name con = do
    g <- newNamedGlobal isConst linkage name
    defineGlobal g con
    return g

type TFunction a = CodeGenModule (Function a)
type TGlobal a = CodeGenModule (Global a)

-- Special string creators
{-# DEPRECATED createString "use withString instead" #-}
createString :: String -> TGlobal (Array n Word8)
createString s = string (length s) (U.constString s)

{-# DEPRECATED createStringNul "use withStringNul instead" #-}
createStringNul :: String -> TGlobal (Array n Word8)
createStringNul s = string (length s + 1) (U.constStringNul s)

withString ::
   String ->
   (forall n. (Nat n) => Global (Array n Word8) -> CodeGenModule a) ->
   CodeGenModule a
withString s act =
   let n = length s
   in  reifyIntegral n (\tn ->
          do arr <- string n (U.constString s)
             act (fixArraySize tn arr))

withStringNul ::
   String ->
   (forall n. (Nat n) => Global (Array n Word8) -> CodeGenModule a) ->
   CodeGenModule a
withStringNul s act =
   let n = length s + 1
   in  reifyIntegral n (\tn ->
          do arr <- string n (U.constStringNul s)
             act (fixArraySize tn arr))

fixArraySize :: n -> Global (Array n a) -> Global (Array n a)
fixArraySize _ = id

string :: Int -> FFI.ValueRef -> TGlobal (Array n Word8)
string n s = do
    modul <- getModule
    name <- genMSym "str"
    let typ = FFI.arrayType (typeRef (undefined :: Word8)) (fromIntegral n)
    liftIO $ liftM Value $ do g <- U.addGlobal modul InternalLinkage name typ
    	     	   	      FFI.setGlobalConstant g 1
			      FFI.setInitializer g s
			      return g

--------------------------------------

-- |Make a constant vector.  Replicates or truncates the list to get length /n/.
constVector :: forall a n . (Pos n) => [ConstValue a] -> ConstValue (Vector n a)
constVector xs =
    ConstValue $ U.constVector (toNum (undefined :: n)) [ v | ConstValue v <- xs ]

-- |Make a constant array.  Replicates or truncates the list to get length /n/.
constArray :: forall a n s . (IsSized a s, Nat n) => [ConstValue a] -> ConstValue (Array n a)
constArray xs =
    ConstValue $ U.constArray (typeRef (undefined :: a)) (toNum (undefined :: n)) [ v | ConstValue v <- xs ]

-- |Make a constant struct.
constStruct :: (IsConstStruct c a) => c -> ConstValue (Struct a)
constStruct struct =
    ConstValue $ U.constStruct (constValueFieldsOf struct) False

-- |Make a constant packed struct.
constPackedStruct :: (IsConstStruct c a) => c -> ConstValue (PackedStruct a)
constPackedStruct struct =
    ConstValue $ U.constStruct (constValueFieldsOf struct) True

class IsConstStruct c a | a -> c, c -> a where
    constValueFieldsOf :: c -> [FFI.ValueRef]

instance (IsConst a, IsConstStruct cs as) => IsConstStruct (ConstValue a, cs) (a, as) where
    constValueFieldsOf (a, as) = unConstValue a : constValueFieldsOf as
instance IsConstStruct () () where
    constValueFieldsOf _ = []
