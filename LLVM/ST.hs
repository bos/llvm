{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, RankNTypes #-}
module LLVM.ST
    ( ModuleGen

    , STModule
    , unsafeFreeze
    , getModule
    , genModule
    , dumpModule

    , STBasicBlock
    , appendBasicBlock

    , STValue
    , dumpValue
    , findGlobal, findFunction
    , addFunction, genFunction

    , STType
    , dumpType
    , findType
    , functionType, intType, structType
    , structCreateNamed, structSetBody

    , CodeGen
    , positionAtEnd, positionBefore, positionAfter

    , getBlock, getFunction, getParams

    , buildRet
    , buildAdd, buildSub, buildMul
    )
    where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.ST.Safe
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)

import qualified LLVM.Wrapper.Core as W
import LLVM.Wrapper.Core ( Module, BasicBlock, Type, Value, Builder, CUInt )
import LLVM.Wrapper.BitWriter

newtype STModule s = STM Module
newtype STBasicBlock s = STB BasicBlock
newtype STType s = STT { unSTT :: Type }
newtype STValue s = STV Value

unsafeFreeze :: STModule s -> ModuleGen s Module
unsafeFreeze (STM m) = return m

dumpModule :: STModule s -> ST s String
dumpModule (STM m) = unsafeIOToST . W.dumpModuleToString $ m

dumpType :: STType s -> ST s String
dumpType (STT t) = unsafeIOToST . W.dumpTypeToString $ t

dumpValue :: STValue s -> ST s String
dumpValue (STV v) = unsafeIOToST . W.dumpValueToString $ v

functionType :: STType s -> [STType s] -> Bool -> STType s
functionType (STT ret) args variadic =
    STT (W.functionType ret (map unSTT args) variadic)

intType :: CUInt -> STType s
intType i = STT (W.integerType i)

structType :: [STType s] -> Bool -> STType s
structType types packed = STT (W.structType (map unSTT types) packed)

newtype ModuleGen s a = MG { unMG :: ReaderT Module (ST s) a }

instance Functor (ModuleGen s) where
    fmap f (MG g) = MG (fmap f g)

instance Applicative (ModuleGen s) where
    pure x = MG (return x)
    (<*>) (MG f) (MG x) = MG (f <*> x)

instance Monad (ModuleGen s) where
    (>>=) (MG x) f = MG (x >>= unMG . f)
    return x = MG (return x)

getModule :: ModuleGen s (STModule s)
getModule = fmap STM (MG ask)

genModule :: String -> ModuleGen s a -> ST s a
genModule name (MG mg) = unsafeIOToST (W.moduleCreateWithName name >>= (unsafeSTToIO . runReaderT mg))

wrapMG :: IO a -> ModuleGen s a
wrapMG = MG . lift . unsafeIOToST

-- FIXME: The following two are per-context, not per-module
structCreateNamed :: String -> ModuleGen s (STType s)
structCreateNamed = fmap STT . wrapMG . W.structCreateNamed

structSetBody :: STType s -> [STType s] -> Bool -> ModuleGen s ()
structSetBody (STT struct) body packed = wrapMG $ W.structSetBody struct (map unSTT body) packed

findType :: String -> ModuleGen s (Maybe (STType s))
findType name = MG ask >>= ((fmap . fmap) STT . wrapMG . flip W.getTypeByName name)

findGlobal :: String -> ModuleGen s (Maybe (STValue s))
findGlobal name = MG ask >>= ((fmap . fmap) STV . wrapMG . flip W.getNamedGlobal name)

findFunction :: String -> ModuleGen s (Maybe (STValue s))
findFunction name = MG ask >>= ((fmap . fmap) STV . wrapMG . flip W.getNamedFunction name)

addFunction :: String -> STType s -> ModuleGen s (STValue s)
addFunction name (STT ty) = MG ask >>= (\m -> fmap STV . wrapMG $ W.addFunction m name ty)

appendBasicBlock :: String -> STValue s -> ModuleGen s (STBasicBlock s)
appendBasicBlock name (STV func) = fmap STB . wrapMG $ W.appendBasicBlock func name

newtype CodeGen s a = CG { unCG :: ReaderT Builder (ST s) a }

instance Functor (CodeGen s) where
    fmap f (CG g) = CG (fmap f g)

instance Applicative (CodeGen s) where
    pure x = CG (return x)
    (<*>) (CG f) (CG x) = CG (f <*> x)

instance Monad (CodeGen s) where
    (>>=) (CG x) f = CG (x >>= unCG . f)
    return x = CG (return x)

genFunction :: String -> STType s -> CodeGen s a -> ModuleGen s a
genFunction name ty fg = do f <- addFunction name ty
                            b <- appendBasicBlock "entry" f
                            wrapMG (W.createBuilder >>=
                                         (unsafeSTToIO . runReaderT
                                                           (unCG (positionAtEnd b >> fg))))

wrapCG :: IO a -> CodeGen s a
wrapCG = CG . lift . unsafeIOToST

positionAtEnd :: STBasicBlock s -> CodeGen s ()
positionAtEnd (STB block) = CG ask >>= wrapCG . flip W.positionAtEnd block

positionBefore :: STValue s -> CodeGen s ()
positionBefore (STV v) = CG ask >>= wrapCG . flip W.positionBefore v

positionAfter :: STValue s -> CodeGen s ()
positionAfter (STV v) =
    CG ask >>= (\builder ->
                    wrapCG $ do
                      block <- W.getInstructionParent v
                      W.positionBuilder builder block v)

getBlock :: CodeGen s (STBasicBlock s)
getBlock = CG ask >>= wrapCG . fmap STB . W.getInsertBlock

getFunction :: CodeGen s (STValue s)
getFunction = getBlock >>= (\(STB b) -> wrapCG . fmap STV $ W.getBasicBlockParent b)

getParams :: CodeGen s [STValue s]
getParams = getFunction >>= (\(STV func) -> (fmap . fmap) STV . wrapCG $ W.getParams func)

wrapUn :: (Builder -> Value -> String -> IO Value) ->
           String -> STValue s -> CodeGen s (STValue s)
wrapUn f n (STV x) = do b <- CG ask; fmap STV . wrapCG $ f b x n

buildRet :: STValue s -> CodeGen s (STValue s)
buildRet (STV x) = do b <- CG ask; fmap STV . wrapCG $ W.buildRet b x

wrapBin :: (Builder -> Value -> Value -> String -> IO Value) ->
           String -> STValue s -> STValue s -> CodeGen s (STValue s)
wrapBin f n (STV l) (STV r) = do b <- CG ask; fmap STV . wrapCG $ f b l r n

buildAdd = wrapBin W.buildAdd
buildSub = wrapBin W.buildSub
buildMul = wrapBin W.buildMul
