{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, RankNTypes #-}
module LLVM.ST
    ( LLVM
    , MemoryBuffer
    , createMemoryBufferWithContentsOfFile
    , createMemoryBufferWithSTDIN
    , createMemoryBufferWithMemoryRange
    , createMemoryBufferWithMemoryRangeCopy
    , liftLL
    , run, run2, runLLVM
    , Context
    , W.getGlobalContext
    , W.contextCreate

    , ModuleGen
    , runModuleGen

    , STModule
    , Module
    , unsafeFreeze, unsafeThaw
    , parseBitcode
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

import qualified LLVM.Wrapper.Core as W
import qualified LLVM.Wrapper.Linker as W
import qualified LLVM.Wrapper.BitReader as W
import qualified LLVM.Wrapper.BitWriter as W
import qualified LLVM.Wrapper.Analysis as W
import LLVM.Wrapper.Core ( MemoryBuffer, Context, BasicBlock, Type, Value, Builder, CUInt, Linkage(..)
                         , createMemoryBufferWithContentsOfFile
                         , createMemoryBufferWithSTDIN
                         , createMemoryBufferWithMemoryRange
                         , createMemoryBufferWithMemoryRangeCopy
                         )

newtype Module = PM { unPM :: W.Module }
newtype STModule c s = STM { unSTM :: W.Module }
newtype STBasicBlock c s = STB BasicBlock
newtype STType c s = STT { unSTT :: Type }
newtype STValue c s = STV { unSTV :: Value }

writeBitcodeToFile :: Module -> FilePath -> IO ()
writeBitcodeToFile (PM m) = W.writeBitcodeToFile m

verifyModule :: Module -> Maybe String
verifyModule (PM m) = unsafePerformIO (W.verifyModule m)

instance Show Module where
    show (PM m) = unsafePerformIO $ W.dumpModuleToString m

newtype LLVM c s a = LL { unLL :: ReaderT Context (ST s) a }

class MonadLLVM m where
    getContext :: m c s Context
    liftLL :: LLVM c s a -> m c s a

instance Functor (LLVM c s) where
    fmap f (LL g) = LL (fmap f g)

instance Applicative (LLVM c s) where
    pure x = LL (return x)
    (<*>) (LL f) (LL x) = LL (f <*> x)

instance Monad (LLVM c s) where
    (>>=) (LL x) f = LL (x >>= unLL . f)
    return x = LL (return x)

instance MonadLLVM LLVM where
    getContext = LL ask
    liftLL = id

wrapLL :: IO a -> LLVM c s a
wrapLL = LL . lift . unsafeIOToST

run :: Context -> (forall c s. LLVM c s (STModule c s)) -> Module
run ctx action = runST $ runLLVM ctx (action >>= unsafeFreeze)

run2 :: Context -> (forall c s. LLVM c s (STModule c s, a)) -> (Module, a)
run2 ctx action = runST $ runLLVM ctx (do (m, x) <- action; m' <- unsafeFreeze m; return (m', x))

runLLVM :: Context -> (forall c. LLVM c s a) -> ST s a
runLLVM ctx (LL lm) = runReaderT lm ctx

unsafeFreeze :: STModule c s -> LLVM c s Module
unsafeFreeze (STM m) = return (PM m)

unsafeThaw :: Module -> LLVM c s (STModule c s)
unsafeThaw (PM m) = return $ STM m

showModule :: STModule c s -> LLVM c s String
showModule (STM m) = wrapLL . W.dumpModuleToString $ m

-- Source module is unusable after this
linkModules :: STModule c s -> STModule c s -> LLVM c s (Maybe String)
linkModules (STM dest) (STM src) = wrapLL $ W.linkModules dest src W.DestroySource

parseBitcode :: MemoryBuffer -> LLVM c s (Either String (STModule c s))
parseBitcode buf = do ctx <- getContext
                      (fmap . fmap) STM . wrapLL $ W.parseBitcodeInContext ctx buf

showType :: STType c s -> LLVM c s String
showType (STT t) = wrapLL . W.dumpTypeToString $ t

showValue :: STValue c s -> LLVM c s String
showValue (STV v) = wrapLL . W.dumpValueToString $ v

functionType :: STType c s -> [STType c s] -> Bool -> LLVM c s (STType c s)
functionType (STT ret) args variadic =
    return $ STT (W.functionType ret (map unSTT args) variadic)

intType :: CUInt -> LLVM c s (STType c s)
intType i = do ctx <- getContext
               wrapLL . fmap STT $ W.intTypeInContext ctx i

structType :: [STType c s] -> Bool -> LLVM c s (STType c s)
structType types packed = do ctx <- getContext
                             wrapLL . fmap STT $ W.structTypeInContext ctx (map unSTT types) packed

structCreateNamed :: String -> LLVM c s (STType c s)
structCreateNamed n = getContext >>= wrapLL . fmap STT . (flip W.structCreateNamedInContext n)

structSetBody :: STType c s -> [STType c s] -> Bool -> LLVM c s ()
structSetBody (STT struct) body packed = wrapLL $ W.structSetBody struct (map unSTT body) packed

vectorType :: STType c s -> CUInt -> LLVM c s (STType c s)
vectorType (STT t) count = return $ STT (W.vectorType t count)

arrayType :: STType c s -> CUInt -> LLVM c s (STType c s)
arrayType (STT t) count = return $ STT (W.arrayType t count)

pointerTypeInSpace :: STType c s -> CUInt -> LLVM c s (STType c s)
pointerTypeInSpace (STT t) addrSpace = return $ STT (W.pointerType t addrSpace)

pointerType :: STType c s -> LLVM c s (STType c s)
pointerType ty = pointerTypeInSpace ty 0

getValueName :: STValue c s -> LLVM c s String
getValueName (STV v) = wrapLL $ W.getValueName v

setValueName :: STValue c s -> String -> LLVM c s ()
setValueName (STV v) = wrapLL . W.setValueName v

constString :: String -> Bool -> LLVM c s (STValue c s)
constString str nullTerminated = do
  ctx <- getContext
  wrapLL . fmap STV $ W.constStringInContext ctx str nullTerminated

constStruct :: [STValue c s] -> Bool -> LLVM c s (STValue c s)
constStruct values packed = do
  ctx <- getContext
  wrapLL . fmap STV $ W.constStructInContext ctx (map unSTV values) packed

appendBasicBlock :: String -> STValue c s -> LLVM c s (STBasicBlock c s)
appendBasicBlock name (STV func) = do
  ctx <- getContext
  fmap STB . wrapLL $ W.appendBasicBlockInContext ctx func name

data MGS = MGS { mgModule :: W.Module, mgCtx :: Context }

newtype ModuleGen c s a = MG { unMG :: ReaderT MGS (ST s) a }

instance Functor (ModuleGen c s) where
    fmap f (MG g) = MG (fmap f g)

instance Applicative (ModuleGen c s) where
    pure x = MG (return x)
    (<*>) (MG f) (MG x) = MG (f <*> x)

instance Monad (ModuleGen c s) where
    (>>=) (MG x) f = MG (x >>= unMG . f)
    return x = MG (return x)

instance MonadReader (STModule c s) (ModuleGen c s) where
    ask = fmap (STM . mgModule) (MG ask)
    local f (MG mg) = MG (local (\(MGS mod ctx) -> MGS (unSTM . f . STM $ mod) ctx) mg)

instance MonadLLVM ModuleGen where
    getContext = fmap mgCtx $ MG ask
    liftLL (LL s) = do ctx <- getContext
                       MG (lift $ runReaderT s ctx)

-- Internal
unsafeMod :: ModuleGen c s W.Module
unsafeMod = fmap mgModule $ MG ask

getModule :: ModuleGen c s (STModule c s)
getModule = ask

genModule :: String -> ModuleGen c s a -> LLVM c s a
genModule name (MG mg) = do
  ctx <- getContext
  wrapLL $ do
    mod <- W.moduleCreateWithNameInContext name ctx
    unsafeSTToIO . runReaderT mg $ MGS mod ctx

runModuleGen :: STModule c s -> ModuleGen c s a -> LLVM c s a
runModuleGen (STM mod) (MG mg) = do
  ctx <- getContext
  LL . lift . runReaderT mg $ MGS mod ctx

wrapMG :: IO a -> ModuleGen c s a
wrapMG = MG . lift . unsafeIOToST

findType :: String -> ModuleGen c s (Maybe (STType c s))
findType name = unsafeMod >>= ((fmap . fmap) STT . wrapMG . flip W.getTypeByName name)

findGlobal :: String -> ModuleGen c s (Maybe (STValue c s))
findGlobal name = unsafeMod >>= ((fmap . fmap) STV . wrapMG . flip W.getNamedGlobal name)

findFunction :: String -> ModuleGen c s (Maybe (STValue c s))
findFunction name = unsafeMod >>= ((fmap . fmap) STV . wrapMG . flip W.getNamedFunction name)

addFunction :: String -> STType c s -> ModuleGen c s (STValue c s)
addFunction name (STT ty) = unsafeMod >>= (\m -> fmap STV . wrapMG $ W.addFunction m name ty)

getLinkage :: STValue c s -> LLVM c s Linkage
getLinkage (STV v) = wrapLL (W.getLinkage v)

setLinkage :: STValue c s -> Linkage -> LLVM c s ()
setLinkage (STV v) l = wrapLL (W.setLinkage v l)

data CGS = CGS { cgBuilder :: Builder, cgMGS :: MGS }

newtype CodeGen c s a = CG { unCG :: ReaderT CGS (ST s) a }

instance Functor (CodeGen c s) where
    fmap f (CG g) = CG (fmap f g)

instance Applicative (CodeGen c s) where
    pure x = CG (return x)
    (<*>) (CG f) (CG x) = CG (f <*> x)

instance Monad (CodeGen c s) where
    (>>=) (CG x) f = CG (x >>= unCG . f)
    return x = CG (return x)

instance MonadLLVM CodeGen where
    getContext = fmap (mgCtx . cgMGS) $ CG ask
    liftLL (LL s) = do ctx <- getContext
                       CG (lift $ runReaderT s ctx)

liftMG :: ModuleGen c s a -> CodeGen c s a
liftMG (MG mg) = do r <- CG ask
                    CG (lift $ runReaderT mg (cgMGS r))

genFunction :: String -> STType c s -> CodeGen c s a -> ModuleGen c s a
genFunction name ty fg =
    do f <- addFunction name ty
       bb <- liftLL $ appendBasicBlock "entry" f
       mgs <- MG ask
       wrapMG (do b <- W.createBuilderInContext (mgCtx mgs)
                  unsafeSTToIO (runReaderT (unCG (positionAtEnd bb >> fg)) (CGS b mgs)))

verifyFunction :: STValue c s -> LLVM c s Bool
verifyFunction (STV f) = wrapLL (W.verifyFunction f)

wrapCG :: IO a -> CodeGen c s a
wrapCG = CG . lift . unsafeIOToST

positionAtEnd :: STBasicBlock c s -> CodeGen c s ()
positionAtEnd (STB block) = CG ask >>= wrapCG . flip W.positionAtEnd block . cgBuilder

positionBefore :: STValue c s -> CodeGen c s ()
positionBefore (STV v) = CG ask >>= wrapCG . flip W.positionBefore v . cgBuilder

positionAfter :: STValue c s -> CodeGen c s ()
positionAfter (STV v) =
    CG ask >>= (\builder ->
                    wrapCG $ do
                      block <- W.getInstructionParent v
                      W.positionBuilder builder block v) . cgBuilder

getBlock :: CodeGen c s (STBasicBlock c s)
getBlock = CG ask >>= wrapCG . fmap STB . W.getInsertBlock . cgBuilder

getFunction :: CodeGen c s (STValue c s)
getFunction = getBlock >>= (\(STB b) -> wrapCG . fmap STV $ W.getBasicBlockParent b)

getParams :: CodeGen c s [STValue c s]
getParams = getFunction >>= (\(STV func) -> (fmap . fmap) STV . wrapCG $ W.getParams func)

wrapUn :: (Builder -> Value -> String -> IO Value) ->
           String -> STValue c s -> CodeGen c s (STValue c s)
wrapUn f n (STV x) = do b <- CG ask; fmap STV . wrapCG $ f (cgBuilder b) x n

buildRet :: STValue c s -> CodeGen c s (STValue c s)
buildRet (STV x) = do b <- CG ask; fmap STV . wrapCG $ W.buildRet (cgBuilder b) x

wrapBin :: (Builder -> Value -> Value -> String -> IO Value) ->
           String -> STValue c s -> STValue c s -> CodeGen c s (STValue c s)
wrapBin f n (STV l) (STV r) = do b <- CG ask; fmap STV . wrapCG $ f (cgBuilder b) l r n

buildAdd = wrapBin W.buildAdd
buildSub = wrapBin W.buildSub
buildMul = wrapBin W.buildMul
