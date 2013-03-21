{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, RankNTypes, NoMonomorphismRestriction #-}
module LLVM.ST
    ( CUInt, CULLong
    , IntPredicate(..), FPPredicate(..)

    , LLVM, MonadLLVM
    , getContext, liftLL, liftST
    , MemoryBuffer
    , createMemoryBufferWithContentsOfFile
    , createMemoryBufferWithSTDIN
    , createMemoryBufferWithMemoryRange
    , createMemoryBufferWithMemoryRangeCopy
    , run, run2, runLLVM
    , Context
    , W.getGlobalContext
    , W.contextCreate

    , ModuleGen, MonadMG
    , runModuleGen, liftMG

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
    , typeOf
    , findGlobal, findFunction
    , addFunction, genFunction, defineFunction, runCodeGen
    , getFuncCallConv, setFuncCallConv, setInstrCallConv
    , getFunctionParams
    , addParamAttrib, addFuncAttrib, removeAttrib
    , getLinkage, setLinkage
    , getTailCall, setTailCall
    , verifyFunction
    , getUndef
    , isConstant, isNull, isUndef, isUnreachable
    , constInt
    , constPtrNull
    , constString, constStruct

    , STType
    , TypeKind(..)
    , typeKind
    , showType
    , findType
    , sizeOf
    , intType, floatType, doubleType, voidType
    , functionType, structType, vectorType, arrayType
    , pointerTypeInSpace, pointerType
    , structCreateNamed, structSetBody

    , CodeGen, MonadCG
    , liftCG
    , position, positionAtEnd, positionBefore, positionAfter
    , getEntryBasicBlock, getNextBasicBlock
    , getFirstInstruction, getNextInstruction, getPreviousInstruction, getLastInstruction
    , getInsertBlock, getFunction, getParams
    , getValueName, setValueName

    , buildTrunc
    , buildZExt
    , buildSExt
    , buildFPToUI
    , buildFPToSI
    , buildUIToFP
    , buildSIToFP
    , buildFPTrunc
    , buildFPExt
    , buildPtrToInt
    , buildIntToPtr
    , buildBitCast
    , buildPointerCast
    , buildTruncOrBitCast
    , buildZExtOrBitCast
    , buildSExtOrBitCast

    , buildInBoundsGEP
    , buildAlloca
    , buildLoad, buildStore
    , buildCall
    , buildBr, buildCondBr
    , buildSwitch, addCase
    , buildPhi, addIncoming
    , buildCase, buildIf
    , buildRet, buildUnreachable
    , buildAdd
    , buildSub
    , buildMul
    , buildFAdd
    , buildFMul
    , buildFPCast
    , buildFSub
    , buildUDiv
    , buildSDiv
    , buildExactSDiv
    , buildFDiv
    , buildURem
    , buildSRem
    , buildFRem
    , buildShl
    , buildLShr
    , buildAShr
    , buildAnd
    , buildOr
    , buildXor
    , buildNeg
    , buildFNeg
    , buildNot
    , buildNSWAdd
    , buildNSWMul
    , buildNSWNeg
    , buildNSWSub
    , buildNUWAdd
    , buildNUWMul
    , buildNUWNeg
    , buildNUWSub
    , buildICmp, buildFCmp
    , buildGlobalString, buildGlobalStringPtr

    -- , constNeg
    -- , constNot
    , constAdd
    , constSub
    , constMul
    -- , constExactSDiv
    , constFAdd
    , constFMul
    -- , constFNeg
    , constFPCast
    , constFSub
    , constUDiv
    , constSDiv
    , constFDiv
    , constURem
    , constSRem
    , constFRem
    , constAnd
    , constOr
    , constXor
    -- , constICmp
    -- , constFCmp
    , constShl
    , constLShr
    , constAShr
    , constGEP
    , constTrunc
    , constSExt
    , constZExt
    , constFPTrunc
    , constFPExt
    , constUIToFP
    , constSIToFP
    , constFPToUI
    , constFPToSI
    , constPtrToInt
    , constIntToPtr
    , constBitCast
    -- , constSelect
    -- , constExtractElement
    -- , constInsertElement
    -- , constShuffleVector
    -- , constRealOfString
    -- , constNSWMul
    -- , constNSWNeg
    -- , constNSWSub
    -- , constNUWAdd
    -- , constNUWMul
    -- , constNUWNeg
    -- , constNUWSub
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
import LLVM.Wrapper.Core ( MemoryBuffer, Context, BasicBlock, Type, Value, Builder
                         , CUInt, CULLong
                         , TypeKind(..)
                         , Linkage(..)
                         , Attribute(..)
                         , CallingConvention(..)
                         , IntPredicate(..), FPPredicate(..)
                         , createMemoryBufferWithContentsOfFile
                         , createMemoryBufferWithSTDIN
                         , createMemoryBufferWithMemoryRange
                         , createMemoryBufferWithMemoryRangeCopy
                         )

newtype Module = PM { unPM :: W.Module }
    deriving Eq
newtype STModule c s = STM { unSTM :: W.Module }
    deriving Eq
newtype STBasicBlock c s = STB { unSTB :: BasicBlock }
    deriving Eq
newtype STType c s = STT { unSTT :: Type }
    deriving Eq
newtype STValue c s = STV { unSTV :: Value }
    deriving Eq

writeBitcodeToFile :: Module -> FilePath -> IO ()
writeBitcodeToFile (PM m) = W.writeBitcodeToFile m

-- Note: LLVM will sometimes call exit inside this. Probably not intended behavior.
verifyModule :: Module -> Maybe String
verifyModule (PM m) = unsafePerformIO (W.verifyModule m)

instance Show Module where
    show (PM m) = unsafePerformIO $ W.dumpModuleToString m

newtype LLVM c s a = LL { unLL :: ReaderT Context (ST s) a }

class MonadLLVM m where
    getContext :: m c s Context
    liftLL :: LLVM c s a -> m c s a
    liftST :: ST s a -> m c s a

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
    liftST = LL . lift

wrap :: MonadLLVM m => IO a -> m c s a
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

sizeOf :: (Monad (m c s), MonadLLVM m) => STType c s -> m c s (STValue c s)
sizeOf (STT ty) = wrap . fmap STV $ W.sizeOf ty

typeKind :: (Monad (m c s), MonadLLVM m) => STType c s -> m c s TypeKind
typeKind (STT t) = wrap $ W.getTypeKind t

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

doubleType :: (Monad (m c s), MonadLLVM m) => m c s (STType c s)
doubleType = do ctx <- getContext
                wrap . fmap STT $ W.doubleTypeInContext ctx

floatType :: (Monad (m c s), MonadLLVM m) => m c s (STType c s)
floatType = do ctx <- getContext
               wrap . fmap STT $ W.floatTypeInContext ctx

voidType :: (Monad (m c s), MonadLLVM m) => m c s (STType c s)
voidType = do ctx <- getContext
              wrap . fmap STT $ W.voidTypeInContext ctx

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

isConstant :: (Monad (m c s), MonadLLVM m) => STValue c s -> m c s Bool
isConstant (STV v) = wrap $ W.isConstant v

isNull :: (Monad (m c s), MonadLLVM m) => STValue c s -> m c s Bool
isNull (STV v) = wrap $ W.isNull v

isUndef :: (Monad (m c s), MonadLLVM m) => STValue c s -> m c s Bool
isUndef (STV v) = wrap $ W.isUndef v

getUndef :: (Monad (m c s), MonadLLVM m) => STType c s -> m c s (STValue c s)
getUndef = return . STV . W.getUndef . unSTT

constInt :: (Monad (m c s), MonadLLVM m) => STType c s -> CULLong -> Bool -> m c s (STValue c s)
constInt (STT intTy) value signExtend = return . STV $ W.constInt intTy value signExtend

constPtrNull :: (Monad (m c s), MonadLLVM m) => STType c s -> m c s (STValue c s)
constPtrNull (STT ty) = wrap . fmap STV $ W.constPointerNull ty

constString :: (Monad (m c s), MonadLLVM m) => String -> Bool -> m c s (STValue c s)
constString str nullTerminated = do
  ctx <- getContext
  wrap . fmap STV $ W.constStringInContext ctx str nullTerminated

constStruct :: (Monad (m c s), MonadLLVM m) => [STValue c s] -> Bool -> m c s (STValue c s)
constStruct values packed = do
  ctx <- getContext
  wrap . fmap STV $ W.constStructInContext ctx (map unSTV values) packed

appendBasicBlock :: (Monad (m c s), MonadLLVM m) =>
                    String -> STValue c s -> m c s (STBasicBlock c s)
appendBasicBlock name (STV func) = do
  ctx <- getContext
  wrap . fmap STB $ W.appendBasicBlockInContext ctx func name

getFunctionParams :: (Functor (m c s), Monad (m c s), MonadLLVM m) =>
                     STValue c s -> m c s [STValue c s]
getFunctionParams (STV func) = (fmap . fmap) STV . wrap $ W.getParams func

addParamAttrib :: (Monad (m c s), MonadLLVM m) => STValue c s -> Attribute -> m c s ()
addParamAttrib (STV param) = wrap . W.addAttribute param

addFuncAttrib :: (Monad (m c s), MonadLLVM m) => STValue c s -> Attribute -> m c s ()
addFuncAttrib (STV func) = wrap . W.addFunctionAttr func

removeAttrib :: (Monad (m c s), MonadLLVM m) => STValue c s -> Attribute -> m c s ()
removeAttrib (STV val) = wrap . W.removeAttribute val

typeOf :: (Monad (m c s), MonadLLVM m) => STValue c s -> m c s (STType c s)
typeOf (STV v) = wrap . fmap STT $ W.typeOf v

data MGS = MGS { mgModule :: W.Module, mgCtx :: Context }

newtype ModuleGen c s a = MG { unMG :: ReaderT MGS (ST s) a }

class MonadLLVM m => MonadMG m where
    liftMG :: ModuleGen c s a -> m c s a

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
    liftST = MG . lift

instance MonadMG ModuleGen where
    liftMG = id

-- Internal
unsafeMod :: ModuleGen c s W.Module
unsafeMod = fmap mgModule $ MG ask

getModule :: MonadMG m => m c s (STModule c s)
getModule = liftMG ask

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

findType :: MonadMG m => String -> m c s (Maybe (STType c s))
findType name = liftMG $ unsafeMod >>= ((fmap . fmap) STT . wrap . flip W.getTypeByName name)

findGlobal :: MonadMG m => String -> m c s (Maybe (STValue c s))
findGlobal name = liftMG $ unsafeMod >>= ((fmap . fmap) STV . wrap . flip W.getNamedGlobal name)

findFunction :: MonadMG m => String -> m c s (Maybe (STValue c s))
findFunction name = liftMG $ unsafeMod >>= ((fmap . fmap) STV . wrap . flip W.getNamedFunction name)

addFunction :: MonadMG m => String -> STType c s -> m c s (STValue c s)
addFunction name (STT ty) = liftMG $ unsafeMod >>= (\m -> fmap STV . wrap $ W.addFunction m name ty)

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

class MonadMG m => MonadCG m where
    liftCG :: CodeGen c s a -> m c s a

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
    liftST = CG . lift

instance MonadMG CodeGen where
    liftMG (MG mg) =
        do r <- CG ask
           CG (lift $ runReaderT mg (cgMGS r))

instance MonadCG CodeGen where
    liftCG = id

-- Declare, initialize, and define
genFunction :: (Functor (m c s), Monad (m c s), MonadMG m) =>
               String -> STType c s -> CodeGen c s a -> m c s a
genFunction name ty cg = do
  f <- addFunction name ty
  bb <- appendBasicBlock "entry" f
  mgs <- liftMG $ MG ask
  wrap (do b <- W.createBuilderInContext (mgCtx mgs)
           unsafeSTToIO (runReaderT (unCG (positionAtEnd bb >> cg)) (CGS b mgs)))

-- Initialize and define
defineFunction :: (Functor (m c s), Monad (m c s), MonadMG m) =>
                  STValue c s -> CodeGen c s a -> m c s a
defineFunction func cg = do
  bb <- appendBasicBlock "entry" func
  mgs <- liftMG $ MG ask
  wrap (do b <- W.createBuilderInContext (mgCtx mgs)
           unsafeSTToIO (runReaderT (unCG (positionAtEnd bb >> cg)) (CGS b mgs)))

-- Just establish a context
runCodeGen :: (Monad (m c s), MonadMG m) =>
              STValue c s -> CodeGen c s a -> ModuleGen c s a
runCodeGen (STV func) cg = do
  bbs <- wrap $ W.getBasicBlocks func
  let cg' = if null bbs then cg else (positionAtEnd (STB (last bbs)) >> cg)
  mgs <- MG ask
  wrap (do b <- W.createBuilderInContext (mgCtx mgs)
           unsafeSTToIO (runReaderT (unCG cg') (CGS b mgs)))

verifyFunction :: (Monad (m c s), MonadLLVM m) => STValue c s -> m c s Bool
verifyFunction (STV f) = wrap (W.verifyFunction f)

position :: MonadCG m => STBasicBlock c s -> STValue c s -> m c s ()
position (STB block) (STV instr) = liftCG $ do
                                     b <- fmap cgBuilder $ CG ask
                                     wrap $ W.positionBuilder b block instr

positionAtEnd :: MonadCG m => STBasicBlock c s -> m c s ()
positionAtEnd (STB block) = liftCG $ CG ask >>= wrap . flip W.positionAtEnd block . cgBuilder

positionBefore :: MonadCG m => STValue c s -> m c s ()
positionBefore (STV v) = liftCG $ CG ask >>= wrap . flip W.positionBefore v . cgBuilder

positionAfter :: MonadCG m => STValue c s -> m c s ()
positionAfter (STV v) =
    liftCG $ CG ask >>=
               (\builder ->
                    wrap $ do
                      block <- W.getInstructionParent v
                      W.positionBuilder builder block v) . cgBuilder

getFirstInstruction :: MonadLLVM m => STBasicBlock c s -> m c s (STValue c s)
getFirstInstruction (STB b) = wrap . fmap STV . W.getFirstInstruction $ b

getLastInstruction :: MonadLLVM m => STBasicBlock c s -> m c s (STValue c s)
getLastInstruction (STB b) = wrap . fmap STV . W.getLastInstruction $ b

getNextInstruction :: MonadLLVM m => STValue c s -> m c s (STValue c s)
getNextInstruction (STV v) = wrap . fmap STV . W.getNextInstruction $ v

getPreviousInstruction :: MonadLLVM m => STValue c s -> m c s (STValue c s)
getPreviousInstruction (STV v) = wrap . fmap STV . W.getPreviousInstruction $ v

getInsertBlock :: MonadCG m => m c s (STBasicBlock c s)
getInsertBlock = liftCG $ CG ask >>= wrap . fmap STB . W.getInsertBlock . cgBuilder

getEntryBasicBlock :: MonadLLVM m => STValue c s -> m c s (STBasicBlock c s)
getEntryBasicBlock (STV f) = liftLL $ wrap . fmap STB . W.getEntryBasicBlock $ f

getNextBasicBlock :: MonadLLVM m => STBasicBlock c s -> m c s (Maybe (STBasicBlock c s))
getNextBasicBlock (STB b) = liftLL $ wrap . (fmap . fmap) STB . W.getNextBasicBlock $ b

getFunction :: MonadCG m => m c s (STValue c s)
getFunction = liftCG $ getInsertBlock >>= (\(STB b) -> wrap . fmap STV $ W.getBasicBlockParent b)

getParams :: MonadCG m => m c s [STValue c s]
getParams = liftCG $ getFunction >>= getFunctionParams

buildInBoundsGEP :: (Monad (m c s), MonadCG m) => String -> STValue c s -> [STValue c s] -> m c s (STValue c s)
buildInBoundsGEP name (STV aggPtr) indices = do
  b <- liftCG $ fmap cgBuilder (CG ask)
  wrap . fmap STV $ W.buildInBoundsGEP b aggPtr (map unSTV indices) name

constGEP :: (Monad (m c s), MonadCG m) => STValue c s -> [STValue c s] -> m c s (STValue c s)
constGEP (STV aggPtr) indices =
  wrap . fmap STV $ W.constGEP aggPtr (map unSTV indices)

buildAlloca :: (Monad (m c s), MonadCG m) => String -> STType c s -> m c s (STValue c s)
buildAlloca name (STT ty) = do
  b <- liftCG $ fmap cgBuilder (CG ask)
  wrap . fmap STV $ W.buildAlloca b ty name

buildLoad :: (Monad (m c s), MonadCG m) => String -> STValue c s -> m c s (STValue c s)
buildLoad name (STV ptr) = do
  b <- liftCG $ fmap cgBuilder (CG ask)
  wrap . fmap STV $ W.buildLoad b ptr name

buildStore :: (Monad (m c s), MonadCG m) => STValue c s -> STValue c s -> m c s (STValue c s)
buildStore (STV value) (STV ptr) = do
  b <- liftCG $ fmap cgBuilder (CG ask)
  wrap . fmap STV $ W.buildStore b value ptr

buildCall :: (Monad (m c s), MonadCG m) =>
             String -> STValue c s -> [STValue c s] -> m c s (STValue c s)
buildCall name (STV func) args = do
  b <- liftCG $ fmap cgBuilder (CG ask)
  wrap . fmap STV $ W.buildCall b func (map unSTV args) name

buildRet :: (Monad (m c s), MonadCG m) => STValue c s -> m c s (STValue c s)
buildRet (STV x) = do b <- liftCG $ CG ask; wrap . fmap STV $ W.buildRet (cgBuilder b) x

buildBr :: (Monad (m c s), MonadCG m) => STBasicBlock c s -> m c s (STValue c s)
buildBr (STB block) = do
  b <- liftCG $ fmap cgBuilder (CG ask)
  wrap . fmap STV $ W.buildBr b block

buildCondBr :: (Monad (m c s), MonadCG m) =>
               STValue c s -> STBasicBlock c s -> STBasicBlock c s -> m c s (STValue c s)
buildCondBr (STV cond) (STB trueBlock) (STB falseBlock) = do
  b <- liftCG $ fmap cgBuilder (CG ask)
  wrap . fmap STV $ W.buildCondBr b cond trueBlock falseBlock

buildSwitch :: (Monad (m c s), MonadCG m) =>
               STValue c s -> STBasicBlock c s -> CUInt -> m c s (STValue c s)
buildSwitch (STV val) (STB defaultBlock) count = do
  b <- liftCG $ fmap cgBuilder (CG ask)
  wrap . fmap STV $ W.buildSwitch b val defaultBlock count

addCase :: (Monad (m c s), MonadCG m) =>
           STValue c s -> STValue c s -> STBasicBlock c b -> m c s ()
addCase (STV switch) (STV val) (STB block) =
    wrap $ W.addCase switch val block

buildCase :: (Functor (m c s), Monad (m c s), MonadCG m) =>
             STValue c s -> m c s (STValue c s) -> [(STValue c s, m c s (STValue c s))]
          -> m c s (STValue c s)
buildCase value defaultCode alts = do
  func <- getFunction
  defBlock <- appendBasicBlock "caseDefault" func
  switch <- buildSwitch value defBlock (fromIntegral (length alts))
  positionAtEnd defBlock
  defResult <- defaultCode
  defExit <- getInsertBlock
  results <- forM alts $ \(val, cg) ->
             do inBlock <- appendBasicBlock "caseAlt" func
                addCase switch val inBlock
                positionAtEnd inBlock
                result <- cg
                outBlock <- getInsertBlock
                return (result, inBlock, outBlock)
  end <- appendBasicBlock "caseExit" func
  positionAtEnd defBlock
  isUnreachable defResult >>= flip unless (void $ buildBr end)
  forM results $ \(result, _, outBlock) ->
      do unreachable <- isUnreachable result
         unless unreachable $ void $ positionAtEnd outBlock >> buildBr end
  positionAtEnd end
  case results of
    [] -> return defResult
    (result, _, _):_ ->
        do ty <- typeOf result
           phi <- buildPhi "caseResult" ty
           inputs <- filterM (\(r, _, _) -> fmap not $ isUnreachable r)
                     ((defResult, defBlock, defExit):results)
           addIncoming phi (map (\(result, _, outBlock) ->
                                     (result, outBlock))
                            inputs)
           return phi

buildIf :: (Monad (m c s), MonadCG m) =>
           STType c s -> STValue c s -> m c s (STValue c s) -> m c s (STValue c s)
        -> m c s (STValue c s)
buildIf ty cond whenTrue whenFalse = do
  func <- getFunction
  initialBlock <- getInsertBlock

  trueBlock <- appendBasicBlock "ifTrue" func
  positionAtEnd trueBlock
  trueResult <- whenTrue
  trueExit <- getInsertBlock

  falseBlock <- appendBasicBlock "ifFalse" func
  positionAtEnd falseBlock
  falseResult <- whenFalse
  falseExit <- getInsertBlock

  positionAtEnd initialBlock
  buildCondBr cond trueBlock falseBlock

  exitBlock <- appendBasicBlock "ifExit" func
  positionAtEnd trueExit
  buildBr exitBlock
  positionAtEnd falseExit
  buildBr exitBlock

  positionAtEnd exitBlock
  phi <- buildPhi "ifResult" ty
  addIncoming phi [ (trueResult, trueExit)
                  , (falseResult, falseExit)]

  return phi


buildPhi :: (Monad (m c s), MonadCG m) => String -> STType c s -> m c s (STValue c s)
buildPhi name (STT ty) = do
  b <- liftCG $ fmap cgBuilder (CG ask)
  wrap . fmap STV $ W.buildPhi b ty name

addIncoming :: (Monad (m c s), MonadCG m) =>
               STValue c s -> [(STValue c s, STBasicBlock c s)] -> m c s ()
addIncoming (STV phi) incoming =
  wrap $ W.addIncoming phi (map (\(a, b) -> (unSTV a, unSTB b)) incoming)

buildUnreachable :: (Monad (m c s), MonadCG m) => m c s (STValue c s)
buildUnreachable = do b <- liftCG $ CG ask; wrap . fmap STV $ W.buildUnreachable (cgBuilder b)

wrapCast :: (Monad (m c s), MonadCG m) =>
            (Builder -> Value -> Type -> String -> IO Value)
         -> String -> STValue c s -> STType c s -> m c s (STValue c s)
wrapCast f n (STV v) (STT t) =
    do b <- liftCG $ CG ask; wrap . fmap STV $ f (cgBuilder b) v t n

buildTrunc          = wrapCast W.buildTrunc
buildZExt           = wrapCast W.buildZExt
buildSExt           = wrapCast W.buildSExt
buildFPToUI         = wrapCast W.buildFPToUI
buildFPToSI         = wrapCast W.buildFPToSI
buildUIToFP         = wrapCast W.buildUIToFP
buildSIToFP         = wrapCast W.buildSIToFP
buildFPTrunc        = wrapCast W.buildFPTrunc
buildFPExt          = wrapCast W.buildFPExt
buildPtrToInt       = wrapCast W.buildPtrToInt
buildIntToPtr       = wrapCast W.buildIntToPtr
buildBitCast        = wrapCast W.buildBitCast
buildPointerCast    = wrapCast W.buildPointerCast
buildTruncOrBitCast = wrapCast W.buildTruncOrBitCast
buildZExtOrBitCast  = wrapCast W.buildZExtOrBitCast
buildSExtOrBitCast  = wrapCast W.buildSExtOrBitCast
buildFPCast         = wrapCast W.buildFPCast

wrapUn :: (Monad (m c s), MonadCG m) =>
          (Builder -> Value -> String -> IO Value)
       -> String -> STValue c s -> m c s (STValue c s)
wrapUn f n (STV x) = do b <- liftCG $ CG ask; wrap . fmap STV $ f (cgBuilder b) x n

buildNeg    = wrapUn W.buildNeg
buildFNeg   = wrapUn W.buildFNeg
buildNot    = wrapUn W.buildNot
buildNSWNeg = wrapUn W.buildNSWNeg
buildNUWNeg = wrapUn W.buildNUWNeg

wrapBin :: (Monad (m c s), MonadCG m) =>
           (Builder -> Value -> Value -> String -> IO Value)
        -> String -> STValue c s -> STValue c s -> m c s (STValue c s)
wrapBin f n (STV l) (STV r) = do b <- liftCG $ CG ask; wrap . fmap STV $ f (cgBuilder b) l r n

buildAdd       = wrapBin W.buildAdd
buildSub       = wrapBin W.buildSub
buildMul       = wrapBin W.buildMul
buildNSWAdd    = wrapBin W.buildNSWAdd
buildNSWSub    = wrapBin W.buildNSWSub
buildNSWMul    = wrapBin W.buildNSWMul
buildNUWAdd    = wrapBin W.buildNUWAdd
buildNUWSub    = wrapBin W.buildNUWSub
buildNUWMul    = wrapBin W.buildNUWMul
buildUDiv      = wrapBin W.buildUDiv
buildSDiv      = wrapBin W.buildSDiv
buildExactSDiv = wrapBin W.buildExactSDiv
buildURem      = wrapBin W.buildURem
buildSRem      = wrapBin W.buildSRem
buildFAdd      = wrapBin W.buildFAdd
buildFSub      = wrapBin W.buildFSub
buildFMul      = wrapBin W.buildFMul
buildFDiv      = wrapBin W.buildFDiv
buildFRem      = wrapBin W.buildFRem
buildShl       = wrapBin W.buildShl
buildLShr      = wrapBin W.buildLShr
buildAShr      = wrapBin W.buildAShr
buildAnd       = wrapBin W.buildAnd
buildOr        = wrapBin W.buildOr
buildXor       = wrapBin W.buildXor

buildICmp :: (Monad (m c s), MonadCG m) =>
             String -> IntPredicate -> STValue c s -> STValue c s -> m c s (STValue c s)
buildICmp name pred (STV l) (STV r) = do
  b <- liftCG $ fmap cgBuilder (CG ask)
  wrap . fmap STV $ W.buildICmp b pred l r name

buildFCmp :: (Monad (m c s), MonadCG m) =>
             String -> FPPredicate -> STValue c s -> STValue c s -> m c s (STValue c s)
buildFCmp name pred (STV l) (STV r) = do
  b <- liftCG $ fmap cgBuilder (CG ask)
  wrap . fmap STV $ W.buildFCmp b pred l r name

wrapConstBin :: (Monad (m c s), MonadLLVM m) =>
                (Value -> Value -> Value)
             -> STValue c s -> STValue c s -> m c s (STValue c s)
wrapConstBin f (STV l) (STV r) = return . STV $ f l r

constAdd       = wrapConstBin W.constAdd
constSub       = wrapConstBin W.constSub
constMul       = wrapConstBin W.constMul
-- constNSWAdd    = wrapConstBin W.constNSWAdd
-- constNSWSub    = wrapConstBin W.constNSWSub
-- constNSWMul    = wrapConstBin W.constNSWMul
-- constNUWAdd    = wrapConstBin W.constNUWAdd
-- constNUWSub    = wrapConstBin W.constNUWSub
-- constNUWMul    = wrapConstBin W.constNUWMul
constUDiv      = wrapConstBin W.constUDiv
constSDiv      = wrapConstBin W.constSDiv
--constExactSDiv = wrapConstBin W.constExactSDiv
constURem      = wrapConstBin W.constURem
constSRem      = wrapConstBin W.constSRem
constFAdd      = wrapConstBin W.constFAdd
constFSub      = wrapConstBin W.constFSub
constFMul      = wrapConstBin W.constFMul
constFDiv      = wrapConstBin W.constFDiv
constFRem      = wrapConstBin W.constFRem
constShl       = wrapConstBin W.constShl
constLShr      = wrapConstBin W.constLShr
constAShr      = wrapConstBin W.constAShr
constAnd       = wrapConstBin W.constAnd
constOr        = wrapConstBin W.constOr
constXor       = wrapConstBin W.constXor

buildGlobalString :: (Monad (m c s), MonadCG m) => String -> String -> m c s (STValue c s)
buildGlobalString name value = do
  b <- liftCG $ fmap cgBuilder (CG ask)
  wrap . fmap STV $ W.buildGlobalString b value name

buildGlobalStringPtr :: (Monad (m c s), MonadCG m) => String -> String -> m c s (STValue c s)
buildGlobalStringPtr name value = do
  b <- liftCG $ fmap cgBuilder (CG ask)
  wrap . fmap STV $ W.buildGlobalStringPtr b value name

wrapConstCast :: (Monad (m c s), MonadLLVM m) =>
                 (Value -> Type -> Value)
              -> STValue c s -> STType c s -> m c s (STValue c s)
wrapConstCast f (STV v) (STT t) = liftLL $ return . STV $ f v t

constTrunc          = wrapConstCast W.constTrunc
constZExt           = wrapConstCast W.constZExt
constSExt           = wrapConstCast W.constSExt
constFPToUI         = wrapConstCast W.constFPToUI
constFPToSI         = wrapConstCast W.constFPToSI
constUIToFP         = wrapConstCast W.constUIToFP
constSIToFP         = wrapConstCast W.constSIToFP
constFPTrunc        = wrapConstCast W.constFPTrunc
constFPExt          = wrapConstCast W.constFPExt
constPtrToInt       = wrapConstCast W.constPtrToInt
constIntToPtr       = wrapConstCast W.constIntToPtr
constBitCast        = wrapConstCast W.constBitCast
-- constPointerCast    = wrapConstCast W.constPointerCast
-- constTruncOrBitCast = wrapConstCast W.constTruncOrBitCast
-- constZExtOrBitCast  = wrapConstCast W.constZExtOrBitCast
-- constSExtOrBitCast  = wrapConstCast W.constSExtOrBitCast
constFPCast         = wrapConstCast W.constFPCast
