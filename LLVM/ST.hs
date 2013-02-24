{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, RankNTypes #-}
module LLVM.ST
    ( LLVM
    , liftLLVM
    , runLLVM
    , Context
    , W.getGlobalContext
    , W.contextCreate

    , ModuleGen
    , run, run2

    , STModule
    , Module
    , unsafeFreeze, unsafeThaw
    , parseBitcode, parseBitcodeForGen, parseBitcodeFromFile
    , writeBitcodeToFile
    , getModule
    , genModule
    , verifyModule
    , showModule
    , linkModules

    , STBasicBlock
    , appendBasicBlock

    , STValue
    , Linkage(..)
    , showValue
    , findGlobal, findFunction
    , addFunction, genFunction
    , getLinkage, setLinkage
    , verifyFunction
    , constString, constStruct

    , STType
    , showType
    , findType
    , functionType, intType, structType
    , vectorType, arrayType
    , pointerTypeInSpace, pointerType
    , structCreateNamed, structSetBody

    , CodeGen
    , liftMG
    , positionAtEnd, positionBefore, positionAfter
    , getBlock, getFunction, getParams
    , getValueName, setValueName

    , buildRet
    , buildAdd, buildSub, buildMul
    )
    where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.ST.Safe
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import System.IO.Unsafe (unsafePerformIO)

import Data.ByteString (ByteString)

import qualified LLVM.Wrapper.Core as W
import qualified LLVM.Wrapper.Linker as W
import qualified LLVM.Wrapper.BitReader as W
import qualified LLVM.Wrapper.BitWriter as W
import qualified LLVM.Wrapper.Analysis as W
import LLVM.Wrapper.Core (Context, BasicBlock, Type, Value, Builder, CUInt, Linkage(..))

newtype Module = PM { unPM :: W.Module }
newtype STModule s = STM { unSTM :: W.Module }
newtype STBasicBlock s = STB BasicBlock
newtype STType s = STT { unSTT :: Type }
newtype STValue s = STV { unSTV :: Value }

unsafeFreeze :: STModule s -> ST s Module
unsafeFreeze (STM m) = return (PM m)

unsafeThaw :: Module -> ST s (STModule s)
unsafeThaw (PM m) = return $ STM m

showModule :: STModule s -> ST s String
showModule (STM m) = unsafeIOToST . W.dumpModuleToString $ m

-- Source module is unusable after this
linkModules :: STModule s -> STModule s -> ST s (Maybe String)
linkModules (STM dest) (STM src) = unsafeIOToST $ W.linkModules dest src W.DestroySource

parseBitcode :: ByteString -> Either String Module
parseBitcode bs = fmap PM $ W.parseBitcode bs

parseBitcodeForGen :: ByteString -> ST s (Either String (STModule s))
parseBitcodeForGen = return . fmap (STM . unPM) . parseBitcode

parseBitcodeFromFile :: FilePath -> IO (Either String Module)
parseBitcodeFromFile path = (fmap . fmap) PM $ W.parseBitcodeFromFile path

writeBitcodeToFile :: Module -> FilePath -> IO ()
writeBitcodeToFile (PM m) = W.writeBitcodeToFile m

verifyModule :: Module -> Maybe String
verifyModule (PM m) = unsafePerformIO (W.verifyModule m)

instance Show Module where
    show (PM m) = unsafePerformIO $ W.dumpModuleToString m

showType :: STType s -> ST s String
showType (STT t) = unsafeIOToST . W.dumpTypeToString $ t

showValue :: STValue s -> ST s String
showValue (STV v) = unsafeIOToST . W.dumpValueToString $ v

newtype LLVM s a = LM { unLM :: ReaderT Context (ST s) a }

class MonadLLVM m where
    getContext :: m s Context
    liftLLVM :: LLVM s a -> m s a

instance Functor (LLVM s) where
    fmap f (LM g) = LM (fmap f g)

instance Applicative (LLVM s) where
    pure x = LM (return x)
    (<*>) (LM f) (LM x) = LM (f <*> x)

instance Monad (LLVM s) where
    (>>=) (LM x) f = LM (x >>= unLM . f)
    return x = LM (return x)

instance MonadLLVM LLVM where
    getContext = LM ask
    liftLLVM = id

wrapLM :: IO a -> LLVM s a
wrapLM = LM . lift . unsafeIOToST

runLLVM :: Context -> LLVM s a -> ST s a
runLLVM ctx (LM lm) = runReaderT lm ctx

functionType :: STType s -> [STType s] -> Bool -> STType s
functionType (STT ret) args variadic =
    STT (W.functionType ret (map unSTT args) variadic)

intType :: CUInt -> LLVM s (STType s)
intType i = do ctx <- getContext
               wrapLM . fmap STT $ W.intTypeInContext ctx i

structType :: [STType s] -> Bool -> LLVM s (STType s)
structType types packed = do ctx <- getContext
                             wrapLM . fmap STT $ W.structTypeInContext ctx (map unSTT types) packed

structCreateNamed :: String -> LLVM s (STType s)
structCreateNamed n = getContext >>= wrapLM . fmap STT . (flip W.structCreateNamedInContext n)

structSetBody :: STType s -> [STType s] -> Bool -> ST s ()
structSetBody (STT struct) body packed = unsafeIOToST $ W.structSetBody struct (map unSTT body) packed

vectorType :: STType s -> CUInt -> STType s
vectorType (STT t) count = STT (W.vectorType t count)

arrayType :: STType s -> CUInt -> STType s
arrayType (STT t) count = STT (W.arrayType t count)

pointerTypeInSpace :: STType s -> CUInt -> STType s
pointerTypeInSpace (STT t) addrSpace = STT (W.pointerType t addrSpace)

pointerType :: STType s -> STType s
pointerType ty = pointerTypeInSpace ty 0

constString :: String -> Bool -> LLVM s (STValue s)
constString str nullTerminated = do
  ctx <- getContext
  wrapLM . fmap STV $ W.constStringInContext ctx str nullTerminated

constStruct :: [STValue s] -> Bool -> LLVM s (STValue s)
constStruct values packed = do
  ctx <- getContext
  wrapLM . fmap STV $ W.constStructInContext ctx (map unSTV values) packed

appendBasicBlock :: String -> STValue s -> LLVM s (STBasicBlock s)
appendBasicBlock name (STV func) = do
  ctx <- getContext
  fmap STB . wrapLM $ W.appendBasicBlockInContext ctx func name

data MGS = MGS { mgModule :: W.Module, mgCtx :: Context }

newtype ModuleGen s a = MG { unMG :: ReaderT MGS (ST s) a }

instance Functor (ModuleGen s) where
    fmap f (MG g) = MG (fmap f g)

instance Applicative (ModuleGen s) where
    pure x = MG (return x)
    (<*>) (MG f) (MG x) = MG (f <*> x)

instance Monad (ModuleGen s) where
    (>>=) (MG x) f = MG (x >>= unMG . f)
    return x = MG (return x)

instance MonadReader (STModule s) (ModuleGen s) where
    ask = fmap (STM . mgModule) (MG ask)
    local f (MG mg) = MG (local (\(MGS mod ctx) -> MGS (unSTM . f . STM $ mod) ctx) mg)

instance MonadLLVM ModuleGen where
    getContext = fmap mgCtx $ MG ask
    liftLLVM (LM s) = do ctx <- getContext
                         MG (lift $ runReaderT s ctx)

-- Internal
unsafeMod :: ModuleGen s W.Module
unsafeMod = fmap mgModule $ MG ask

getModule :: ModuleGen s (STModule s)
getModule = ask

genModule :: String -> ModuleGen s a -> LLVM s a
genModule name (MG mg) = do
  ctx <- getContext
  wrapLM $ do
    mod <- W.moduleCreateWithNameInContext name ctx
    unsafeSTToIO . runReaderT mg $ MGS mod ctx

run :: (forall s. ST s (STModule s)) -> Module
run action = runST (action >>= unsafeFreeze)

run2 :: (forall s. ST s (STModule s, a)) -> (Module, a)
run2 action = runST (do (m, x) <- action; m' <- unsafeFreeze m; return (m', x))

wrapMG :: IO a -> ModuleGen s a
wrapMG = MG . lift . unsafeIOToST

findType :: String -> ModuleGen s (Maybe (STType s))
findType name = unsafeMod >>= ((fmap . fmap) STT . wrapMG . flip W.getTypeByName name)

findGlobal :: String -> ModuleGen s (Maybe (STValue s))
findGlobal name = unsafeMod >>= ((fmap . fmap) STV . wrapMG . flip W.getNamedGlobal name)

findFunction :: String -> ModuleGen s (Maybe (STValue s))
findFunction name = unsafeMod >>= ((fmap . fmap) STV . wrapMG . flip W.getNamedFunction name)

addFunction :: String -> STType s -> ModuleGen s (STValue s)
addFunction name (STT ty) = unsafeMod >>= (\m -> fmap STV . wrapMG $ W.addFunction m name ty)

getLinkage :: STValue s -> ST s Linkage
getLinkage (STV v) = unsafeIOToST (W.getLinkage v)

setLinkage :: STValue s -> Linkage -> ST s ()
setLinkage (STV v) l = unsafeIOToST (W.setLinkage v l)

data CGS = CGS { cgBuilder :: Builder, cgMGS :: MGS }

newtype CodeGen s a = CG { unCG :: ReaderT CGS (ST s) a }

instance Functor (CodeGen s) where
    fmap f (CG g) = CG (fmap f g)

instance Applicative (CodeGen s) where
    pure x = CG (return x)
    (<*>) (CG f) (CG x) = CG (f <*> x)

instance Monad (CodeGen s) where
    (>>=) (CG x) f = CG (x >>= unCG . f)
    return x = CG (return x)

liftMG :: ModuleGen s a -> CodeGen s a
liftMG (MG mg) = do r <- CG ask
                    CG (lift $ runReaderT mg (cgMGS r))

genFunction :: String -> STType s -> CodeGen s a -> ModuleGen s a
genFunction name ty fg =
    do f <- addFunction name ty
       bb <- liftLLVM $ appendBasicBlock "entry" f
       mgs <- MG ask
       wrapMG (do b <- W.createBuilderInContext (mgCtx mgs)
                  unsafeSTToIO (runReaderT (unCG (positionAtEnd bb >> fg)) (CGS b mgs)))

verifyFunction :: STValue s -> ST s Bool
verifyFunction (STV f) = unsafeIOToST (W.verifyFunction f)

wrapCG :: IO a -> CodeGen s a
wrapCG = CG . lift . unsafeIOToST

positionAtEnd :: STBasicBlock s -> CodeGen s ()
positionAtEnd (STB block) = CG ask >>= wrapCG . flip W.positionAtEnd block . cgBuilder

positionBefore :: STValue s -> CodeGen s ()
positionBefore (STV v) = CG ask >>= wrapCG . flip W.positionBefore v . cgBuilder

positionAfter :: STValue s -> CodeGen s ()
positionAfter (STV v) =
    CG ask >>= (\builder ->
                    wrapCG $ do
                      block <- W.getInstructionParent v
                      W.positionBuilder builder block v) . cgBuilder

getBlock :: CodeGen s (STBasicBlock s)
getBlock = CG ask >>= wrapCG . fmap STB . W.getInsertBlock . cgBuilder

getFunction :: CodeGen s (STValue s)
getFunction = getBlock >>= (\(STB b) -> wrapCG . fmap STV $ W.getBasicBlockParent b)

getParams :: CodeGen s [STValue s]
getParams = getFunction >>= (\(STV func) -> (fmap . fmap) STV . wrapCG $ W.getParams func)

getValueName :: STValue s -> CodeGen s String
getValueName (STV v) = wrapCG $ W.getValueName v

setValueName :: STValue s -> String -> CodeGen s ()
setValueName (STV v) = wrapCG . W.setValueName v

wrapUn :: (Builder -> Value -> String -> IO Value) ->
           String -> STValue s -> CodeGen s (STValue s)
wrapUn f n (STV x) = do b <- CG ask; fmap STV . wrapCG $ f (cgBuilder b) x n

buildRet :: STValue s -> CodeGen s (STValue s)
buildRet (STV x) = do b <- CG ask; fmap STV . wrapCG $ W.buildRet (cgBuilder b) x

wrapBin :: (Builder -> Value -> Value -> String -> IO Value) ->
           String -> STValue s -> STValue s -> CodeGen s (STValue s)
wrapBin f n (STV l) (STV r) = do b <- CG ask; fmap STV . wrapCG $ f (cgBuilder b) l r n

buildAdd = wrapBin W.buildAdd
buildSub = wrapBin W.buildSub
buildMul = wrapBin W.buildMul
