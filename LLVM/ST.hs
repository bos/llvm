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
    , Attribute(..)
    , CallingConvention(..)
    , showValue
    , findGlobal, findFunction
    , addFunction, genFunction, defineFunction, runCodeGen
    , getFuncCallConv, setFuncCallConv, setInstrCallConv
    , getFunctionParams
    , addParamAttrib, addFuncAttrib, removeAttrib
    , getLinkage, setLinkage
    , getTailCall, setTailCall
    , verifyFunction
    , getUndef, isUnreachable
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

    , buildCall
    , buildRet, buildUnreachable
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
                         , Attribute(..)
                         , CallingConvention(..)
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

wrap :: (Monad (m c s), MonadLLVM m) => IO a -> m c s a
wrap = liftLL . LL . lift . unsafeIOToST

run :: Context -> (forall c s. LLVM c s (STModule c s)) -> Module
run ctx action = runST $ runLLVM ctx (action >>= unsafeFreeze)

run2 :: Context -> (forall c s. LLVM c s (STModule c s, a)) -> (Module, a)
run2 ctx action = runST $ runLLVM ctx (do (m, x) <- action; m' <- unsafeFreeze m; return (m', x))

runLLVM :: Context -> (forall c. LLVM c s a) -> ST s a
runLLVM ctx (LL lm) = runReaderT lm ctx

unsafeFreeze :: (Monad (m c s), MonadLLVM m) => STModule c s -> m c s Module
unsafeFreeze (STM m) = liftLL $ return (PM m)

unsafeThaw :: (Monad (m c s), MonadLLVM m) => Module -> m c s (STModule c s)
unsafeThaw (PM m) = liftLL $ return $ STM m

showModule :: (Monad (m c s), MonadLLVM m) => STModule c s -> m c s String
showModule (STM m) = wrap . W.dumpModuleToString $ m

-- Source module is unusable after this
linkModules :: (Monad (m c s), MonadLLVM m) => STModule c s -> STModule c s -> m c s (Maybe String)
linkModules (STM dest) (STM src) = wrap $ W.linkModules dest src W.DestroySource

parseBitcode :: (Functor (m c s), Monad (m c s), MonadLLVM m) =>
                MemoryBuffer -> m c s (Either String (STModule c s))
parseBitcode buf = do ctx <- getContext
                      (fmap . fmap) STM . wrap $ W.parseBitcodeInContext ctx buf

showType :: (Monad (m c s), MonadLLVM m) => STType c s -> m c s String
showType (STT t) = wrap . W.dumpTypeToString $ t

showValue :: (Monad (m c s), MonadLLVM m) => STValue c s -> m c s String
showValue (STV v) = wrap . W.dumpValueToString $ v

functionType :: (Monad (m c s), MonadLLVM m) => STType c s -> [STType c s] -> Bool -> m c s (STType c s)
functionType (STT ret) args variadic =
    liftLL $ return $ STT (W.functionType ret (map unSTT args) variadic)

intType :: (Monad (m c s), MonadLLVM m) => CUInt -> m c s (STType c s)
intType i = do ctx <- getContext
               wrap . fmap STT $ W.intTypeInContext ctx i

structType :: (Monad (m c s), MonadLLVM m) => [STType c s] -> Bool -> m c s (STType c s)
structType types packed = do ctx <- getContext
                             wrap . fmap STT $ W.structTypeInContext ctx (map unSTT types) packed

structCreateNamed :: (Monad (m c s), MonadLLVM m) => String -> m c s (STType c s)
structCreateNamed n = getContext >>= wrap . fmap STT . (flip W.structCreateNamedInContext n)

structSetBody :: (Monad (m c s), MonadLLVM m) => STType c s -> [STType c s] -> Bool -> m c s ()
structSetBody (STT struct) body packed = wrap $ W.structSetBody struct (map unSTT body) packed

vectorType :: (Monad (m c s), MonadLLVM m) => STType c s -> CUInt -> m c s (STType c s)
vectorType (STT t) count = return $ STT (W.vectorType t count)

arrayType :: (Monad (m c s), MonadLLVM m) => STType c s -> CUInt -> m c s (STType c s)
arrayType (STT t) count = return $ STT (W.arrayType t count)

pointerTypeInSpace :: (Monad (m c s), MonadLLVM m) => STType c s -> CUInt -> m c s (STType c s)
pointerTypeInSpace (STT t) addrSpace = return $ STT (W.pointerType t addrSpace)

pointerType :: (Monad (m c s), MonadLLVM m) => STType c s -> m c s (STType c s)
pointerType ty = pointerTypeInSpace ty 0

getValueName :: (Monad (m c s), MonadLLVM m) => STValue c s -> m c s String
getValueName (STV v) = wrap $ W.getValueName v

setValueName :: (Monad (m c s), MonadLLVM m) => STValue c s -> String -> m c s ()
setValueName (STV v) = wrap . W.setValueName v

isUnreachable :: (Monad (m c s), MonadLLVM m) => STValue c s -> m c s Bool
isUnreachable (STV v) = wrap $ W.isUnreachable v

getUndef :: (Monad (m c s), MonadLLVM m) => STType c s -> m c s (STValue c s)
getUndef = return . STV . W.getUndef . unSTT

constString :: (Monad (m c s), MonadLLVM m) => String -> Bool -> m c s (STValue c s)
constString str nullTerminated = do
  ctx <- getContext
  wrap . fmap STV $ W.constStringInContext ctx str nullTerminated

constStruct :: (Monad (m c s), MonadLLVM m) => [STValue c s] -> Bool -> m c s (STValue c s)
constStruct values packed = do
  ctx <- getContext
  wrap . fmap STV $ W.constStructInContext ctx (map unSTV values) packed

appendBasicBlock :: (Functor (m c s), Monad (m c s), MonadLLVM m) =>
                    String -> STValue c s -> m c s (STBasicBlock c s)
appendBasicBlock name (STV func) = do
  ctx <- getContext
  fmap STB . wrap $ W.appendBasicBlockInContext ctx func name

getFunctionParams :: (Functor (m c s), Monad (m c s), MonadLLVM m) =>
                     STValue c s -> m c s [STValue c s]
getFunctionParams (STV func) = (fmap . fmap) STV . wrap $ W.getParams func

addParamAttrib :: (Monad (m c s), MonadLLVM m) => STValue c s -> Attribute -> m c s ()
addParamAttrib (STV param) = wrap . W.addAttribute param

addFuncAttrib :: (Monad (m c s), MonadLLVM m) => STValue c s -> Attribute -> m c s ()
addFuncAttrib (STV func) = wrap . W.addFunctionAttr func

removeAttrib :: (Monad (m c s), MonadLLVM m) => STValue c s -> Attribute -> m c s ()
removeAttrib (STV val) = wrap . W.removeAttribute val

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

genModule :: (Monad (m c s), MonadLLVM m) => String -> ModuleGen c s a -> m c s a
genModule name (MG mg) = do
  ctx <- getContext
  wrap $ do
    mod <- W.moduleCreateWithNameInContext name ctx
    unsafeSTToIO . runReaderT mg $ MGS mod ctx

runModuleGen :: (Monad (m c s), MonadLLVM m) => STModule c s -> ModuleGen c s a -> m c s a
runModuleGen (STM mod) (MG mg) = do
  ctx <- getContext
  liftLL $ LL . lift . runReaderT mg $ MGS mod ctx

findType :: String -> ModuleGen c s (Maybe (STType c s))
findType name = unsafeMod >>= ((fmap . fmap) STT . wrap . flip W.getTypeByName name)

findGlobal :: String -> ModuleGen c s (Maybe (STValue c s))
findGlobal name = unsafeMod >>= ((fmap . fmap) STV . wrap . flip W.getNamedGlobal name)

findFunction :: String -> ModuleGen c s (Maybe (STValue c s))
findFunction name = unsafeMod >>= ((fmap . fmap) STV . wrap . flip W.getNamedFunction name)

addFunction :: String -> STType c s -> ModuleGen c s (STValue c s)
addFunction name (STT ty) = unsafeMod >>= (\m -> fmap STV . wrap $ W.addFunction m name ty)

getLinkage :: (Monad (m c s), MonadLLVM m) => STValue c s -> m c s Linkage
getLinkage (STV v) = wrap (W.getLinkage v)

setLinkage :: (Monad (m c s), MonadLLVM m) => STValue c s -> Linkage -> m c s ()
setLinkage (STV v) = wrap . W.setLinkage v

getTailCall :: (Monad (m c s), MonadLLVM m) => STValue c s -> m c s Bool
getTailCall (STV call) = wrap $ W.isTailCall call

setTailCall :: (Monad (m c s), MonadLLVM m) => STValue c s -> Bool -> m c s ()
setTailCall (STV call) = wrap . W.setTailCall call

setFuncCallConv :: (Monad (m c s), MonadLLVM m) => STValue c s -> CallingConvention -> m c s ()
setFuncCallConv (STV func) = wrap . W.setFunctionCallConv func

getFuncCallConv :: (Monad (m c s), MonadLLVM m) => STValue c s -> m c s CallingConvention
getFuncCallConv (STV func) = wrap $ W.getFunctionCallConv func

setInstrCallConv :: (Monad (m c s), MonadLLVM m) => STValue c s -> CallingConvention -> m c s ()
setInstrCallConv (STV func) = wrap . W.setInstructionCallConv func

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

-- Declare, initialize, and define
genFunction :: String -> STType c s -> CodeGen c s a -> ModuleGen c s a
genFunction name ty cg = do
  f <- addFunction name ty
  bb <- appendBasicBlock "entry" f
  mgs <- MG ask
  wrap (do b <- W.createBuilderInContext (mgCtx mgs)
           unsafeSTToIO (runReaderT (unCG (positionAtEnd bb >> cg)) (CGS b mgs)))

-- Initialize and define
defineFunction :: STValue c s -> CodeGen c s a -> ModuleGen c s a
defineFunction func cg = do
  bb <- appendBasicBlock "entry" func
  mgs <- MG ask
  wrap (do b <- W.createBuilderInContext (mgCtx mgs)
           unsafeSTToIO (runReaderT (unCG (positionAtEnd bb >> cg)) (CGS b mgs)))

-- Just establish a context
runCodeGen :: STValue c s -> CodeGen c s a -> ModuleGen c s a
runCodeGen (STV func) cg = do
  bbs <- wrap $ W.getBasicBlocks func
  let cg' = if null bbs then cg else (positionAtEnd (STB (last bbs)) >> cg)
  mgs <- MG ask
  wrap (do b <- W.createBuilderInContext (mgCtx mgs)
           unsafeSTToIO (runReaderT (unCG cg') (CGS b mgs)))

verifyFunction :: (Monad (m c s), MonadLLVM m) => STValue c s -> m c s Bool
verifyFunction (STV f) = wrap (W.verifyFunction f)

positionAtEnd :: STBasicBlock c s -> CodeGen c s ()
positionAtEnd (STB block) = CG ask >>= wrap . flip W.positionAtEnd block . cgBuilder

positionBefore :: STValue c s -> CodeGen c s ()
positionBefore (STV v) = CG ask >>= wrap . flip W.positionBefore v . cgBuilder

positionAfter :: STValue c s -> CodeGen c s ()
positionAfter (STV v) =
    CG ask >>= (\builder ->
                    wrap $ do
                      block <- W.getInstructionParent v
                      W.positionBuilder builder block v) . cgBuilder

getBlock :: CodeGen c s (STBasicBlock c s)
getBlock = CG ask >>= wrap . fmap STB . W.getInsertBlock . cgBuilder

getFunction :: CodeGen c s (STValue c s)
getFunction = getBlock >>= (\(STB b) -> wrap . fmap STV $ W.getBasicBlockParent b)

getParams :: CodeGen c s [STValue c s]
getParams = getFunction >>= getFunctionParams

buildCall :: String -> STValue c s -> [STValue c s] -> CodeGen c s (STValue c s)
buildCall name (STV func) args = do
  b <- fmap cgBuilder (CG ask)
  wrap . fmap STV $ W.buildCall b func (map unSTV args) name

buildRet :: STValue c s -> CodeGen c s (STValue c s)
buildRet (STV x) = do b <- CG ask; fmap STV . wrap $ W.buildRet (cgBuilder b) x

buildUnreachable :: CodeGen c s (STValue c s)
buildUnreachable = do b <- CG ask; fmap STV . wrap $ W.buildUnreachable (cgBuilder b)

wrapUn :: (Builder -> Value -> String -> IO Value) ->
           String -> STValue c s -> CodeGen c s (STValue c s)
wrapUn f n (STV x) = do b <- CG ask; fmap STV . wrap $ f (cgBuilder b) x n

wrapBin :: (Builder -> Value -> Value -> String -> IO Value) ->
           String -> STValue c s -> STValue c s -> CodeGen c s (STValue c s)
wrapBin f n (STV l) (STV r) = do b <- CG ask; fmap STV . wrap $ f (cgBuilder b) l r n

buildAdd = wrapBin W.buildAdd
buildSub = wrapBin W.buildSub
buildMul = wrapBin W.buildMul
