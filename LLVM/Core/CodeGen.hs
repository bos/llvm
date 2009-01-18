{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances, UndecidableInstances, FlexibleContexts, ScopedTypeVariables #-}
module LLVM.Core.CodeGen(
    -- * Module creation
    newModule, newNamedModule, defineModule, createModule,
    getModuleValues, ModuleValue, castModuleValue,
    -- * Globals
    Linkage(..),
    -- * Function creation
    Function, newFunction, newNamedFunction, defineFunction, createFunction, createNamedFunction,
    addFunctionAttributes,
    FFI.Attribute(..),
    externFunction,
    FunctionArgs, FunctionRet,
    TFunction,
    -- * Global variable creation
    Global, newGlobal, newNamedGlobal, defineGlobal, createGlobal, createNamedGlobal, TGlobal,
    -- * Values
    Value(..), ConstValue(..),
    IsConst(..), valueOf, value,
    zero, allOnes, undef,
    createString, createStringNul,
    constVector, constArray,
    -- * Basic blocks
    BasicBlock(..), newBasicBlock, newNamedBasicBlock, defineBasicBlock, createBasicBlock, getCurrentBasicBlock,
    -- * Misc
    withCurrentBuilder
    ) where
import Control.Monad(liftM, when)
import Data.Int
import Data.Word
import Data.TypeNumbers
import LLVM.Core.CodeGenMonad
import qualified LLVM.FFI.Core as FFI
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

getModuleValues :: U.Module -> IO [(String, ModuleValue)]
getModuleValues = liftM (map (\ (s,p) -> (s, ModuleValue p))) . U.getModuleValues

castModuleValue :: forall a . (IsType a) => ModuleValue -> Maybe (Value a)
castModuleValue (ModuleValue f) =
    if U.valueHasType f (typeRef (undefined :: a)) then Just (Value f) else Nothing

--------------------------------------

newtype Value a = Value { unValue :: FFI.ValueRef }

newtype ConstValue a = ConstValue FFI.ValueRef

-- XXX merge with IsArithmetic?
class (IsArithmetic a) => IsConst a where
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

{-
instance IsConst (Array n a) where
    constOf (Array xs) = 
      withArrayLen xs $ \ len ptr ->
        constArray (typeRef (undefined :: a)) ??? len
-}

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
    liftIO $ liftM Value $ U.addFunction modul (fromIntegral $ fromEnum linkage) name typ

-- | Create a new function.  Use 'newNamedFunction' to create a function with external linkage, since
-- it needs a known name.
newFunction :: forall a . (IsFunction a)
            => Linkage
            -> CodeGenModule (Function a)
newFunction linkage = genMSym "fun" >>= newNamedFunction linkage

-- | Define a function body.  The basic block returned by the function is the function entry point.
defineFunction :: forall f g r . (FunctionArgs f g (CodeGenFunction r ()))
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
createFunction :: (IsFunction f, FunctionArgs f g (CodeGenFunction r ()))
               => Linkage
               -> g  -- ^ Function body.
               -> CodeGenModule (Function f)
createFunction linkage body = do
    f <- newFunction linkage
    defineFunction f body
    return f

-- | Create a new function with the given body.
createNamedFunction :: (IsFunction f, FunctionArgs f g (CodeGenFunction r ()))
               => Linkage
	       -> String
               -> g  -- ^ Function body.
               -> CodeGenModule (Function f)
createNamedFunction linkage name body = do
    f <- newNamedFunction linkage name
    defineFunction f body
    return f

addFunctionAttributes :: Function a -> [FFI.Attribute] -> CodeGenModule ()
addFunctionAttributes (Value f) as =
    liftIO $ mapM_ (FFI.addAttribute f . FFI.fromAttribute) as

-- XXX This is ugly, it must be possible to make it simpler
-- Convert a function of type f = t1->t2->...-> IO r to
-- g = Value t1 -> Value t2 -> ... CodeGenFunction r ()
class FunctionArgs f g r | f -> g r, g r -> f where
    apArgs :: Int -> FunctionRef -> g -> r

applyArgs :: (FunctionArgs f g r) => FunctionRef -> g -> r
applyArgs = apArgs 0

instance (FunctionArgs b b' r) => FunctionArgs (a -> b) (Value a -> b') r where
    apArgs n f g = apArgs (n+1) f (g $ Value $ U.getParam f n)

-- XXX instances for all IsFirstClass functions,
-- because Haskell can't deal with the context and the FD
type FA a = CodeGenFunction a ()
instance FunctionArgs (IO Float)        (FA Float)        (FA Float)        where apArgs _ _ g = g
instance FunctionArgs (IO Double)       (FA Double)       (FA Double)       where apArgs _ _ g = g
instance FunctionArgs (IO FP128)        (FA FP128)        (FA FP128)        where apArgs _ _ g = g
instance (IsTypeNumber n) => 
         FunctionArgs (IO (IntN n))     (FA (IntN n))     (FA (IntN n))     where apArgs _ _ g = g
instance (IsTypeNumber n) =>
         FunctionArgs (IO (WordN n))    (FA (WordN n))    (FA (WordN n))    where apArgs _ _ g = g
instance FunctionArgs (IO Bool)         (FA Bool)         (FA Bool)         where apArgs _ _ g = g
instance FunctionArgs (IO Int8)         (FA Int8)         (FA Int8)         where apArgs _ _ g = g
instance FunctionArgs (IO Int16)        (FA Int16)        (FA Int16)        where apArgs _ _ g = g
instance FunctionArgs (IO Int32)        (FA Int32)        (FA Int32)        where apArgs _ _ g = g
instance FunctionArgs (IO Int64)        (FA Int64)        (FA Int64)        where apArgs _ _ g = g
instance FunctionArgs (IO Word8)        (FA Word8)        (FA Word8)        where apArgs _ _ g = g
instance FunctionArgs (IO Word16)       (FA Word16)       (FA Word16)       where apArgs _ _ g = g
instance FunctionArgs (IO Word32)       (FA Word32)       (FA Word32)       where apArgs _ _ g = g
instance FunctionArgs (IO Word64)       (FA Word64)       (FA Word64)       where apArgs _ _ g = g
instance FunctionArgs (IO ())           (FA ())           (FA ())           where apArgs _ _ g = g
instance (IsTypeNumber n, IsPrimitive a) =>
         FunctionArgs (IO (Vector n a)) (FA (Vector n a)) (FA (Vector n a)) where apArgs _ _ g = g
instance (IsType a) => 
         FunctionArgs (IO (Ptr a))      (FA (Ptr a))      (FA (Ptr a))      where apArgs _ _ g = g

-- |This class is just to simplify contexts.
class (FunctionArgs (IO a) (CodeGenFunction a ()) (CodeGenFunction a ())) => FunctionRet a
instance (FunctionArgs (IO a) (CodeGenFunction a ()) (CodeGenFunction a ())) => FunctionRet a

--------------------------------------

-- |A basic block is a sequence of non-branching instructions, terminated by a control flow instruction.
newtype BasicBlock = BasicBlock FFI.BasicBlockRef

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

--------------------------------------

-- |Create a reference to an external function while code generating for a function.
externFunction :: forall a r . (IsFunction a) => String -> [FFI.Attribute] -> CodeGenFunction r (Function a)
externFunction name as = do
    es <- getExterns
    case lookup name es of
        Just f -> return $ Value f
        Nothing -> do
            let linkage = ExternalLinkage
            modul <- getFunctionModule
            let typ = typeRef (undefined :: a)
            f <- liftIO $ U.addFunction modul (fromIntegral $ fromEnum linkage) name typ
	    liftIO $ mapM_ (FFI.addAttribute f . FFI.fromAttribute) as
            putExterns ((name, f) : es)
	    return $ Value f

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
    liftIO $ liftM Value $ do g <- U.addGlobal modul (fromIntegral $ fromEnum linkage) name typ
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
createString :: String -> TGlobal (Array n Word8)
createString s = string (length s) (U.constString s)

createStringNul :: String -> TGlobal (Array n Word8)
createStringNul s = string (length s + 1) (U.constStringNul s)

string :: Int -> FFI.ValueRef -> TGlobal (Array n Word8)
string n s = do
    modul <- getModule
    name <- genMSym "str"
    let typ = FFI.arrayType (typeRef (undefined :: Word8)) (fromIntegral n)
    liftIO $ liftM Value $ do g <- U.addGlobal modul (fromIntegral $ fromEnum InternalLinkage) name typ
    	     	   	      FFI.setGlobalConstant g 1
			      FFI.setInitializer g s
			      return g

--------------------------------------

-- |An enumeration for the kinds of linkage for global values.
data Linkage
    = ExternalLinkage     -- ^Externally visible function
    | LinkOnceLinkage     -- ^Keep one copy of function when linking (inline)
    | WeakLinkage         -- ^Keep one copy of named function when linking (weak)
    | AppendingLinkage    -- ^Special purpose, only applies to global arrays
    | InternalLinkage     -- ^Rename collisions when linking (static functions)
    | DLLImportLinkage    -- ^Function to be imported from DLL
    | DLLExportLinkage    -- ^Function to be accessible from DLL
    | ExternalWeakLinkage -- ^ExternalWeak linkage description
    | GhostLinkage        -- ^Stand-in functions for streaming fns from BC files    
    deriving (Show, Eq, Ord, Enum)

{-
-- |An enumeration for the kinds of visibility of global values.
data VisibilityTypes
    = DefaultVisibility   -- ^The GV is visible
    | HiddenVisibility    -- ^The GV is hidden
    | ProtectedVisibility -- ^The GV is protected
    deriving (Show, Eq, Ord, Enum)
-}

--------------------------------------

-- |Make a constant vector.  Replicates or truncates the list to get length /n/.
constVector :: forall a n . (IsTypeNumber n) => [ConstValue a] -> ConstValue (Vector n a)
constVector xs =
    ConstValue $ U.constVector (typeNumber (undefined :: n)) [ v | ConstValue v <- xs ]

-- |Make a constant array.  Replicates or truncates the list to get length /n/.
constArray :: forall a n . (IsSized a, IsTypeNumber n) => [ConstValue a] -> ConstValue (Array n a)
constArray xs =
    ConstValue $ U.constArray (typeRef (undefined :: Array n a)) (typeNumber (undefined :: n)) [ v | ConstValue v <- xs ]
