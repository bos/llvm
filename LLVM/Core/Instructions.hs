{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, TypeSynonymInstances, ScopedTypeVariables, OverlappingInstances #-}
module LLVM.Core.Instructions(
    -- * Terminator instructions
    ret,
    condBr,
    br,
    switch,
    invoke,
    unwind,
    unreachable,
    -- * Arithmetic binary operations
    add, sub, mul,
    udiv, sdiv, fdiv, urem, srem, frem,
    -- * Logical binary operations
    shl, lshr, ashr, and, or, xor,
    -- * Vector operations
    extractelement,
    insertelement,
    shufflevector,
    -- * Memory access
    malloc, arrayMalloc,
    alloca, arrayAlloca,
    free,
    load,
    store,
    getElementPtr,
    -- * Conversions
    trunc, zext, sext,
    fptrunc, fpext,
    fptoui, fptosi,
    uitofp, sitofp,
    ptrtoint, inttoptr,
    bitcast,
    -- * Other
    IntPredicate(..), RealPredicate(..),
    icmp, fcmp,
    phi,
    select,
    call,
    -- va_arg
    -- * Classes and types
    Terminate,
    Ret, CallArgs, ABinOp, CmpOp, FunctionArgs, IsConst
    ) where
import Prelude hiding (and, or)
import Control.Monad(liftM)
--import Data.Int
import Data.Word
--import Data.TypeNumbers
import qualified LLVM.Core.FFI as FFI
import LLVM.Core.Data
import LLVM.Core.Type
import LLVM.Core.CodeGenMonad
import LLVM.Core.CodeGen
import qualified LLVM.Core.Util as U

-- TODO:
-- Add vector version of arithmetic
-- Add rest of instructions
-- Use Terminate to ensure bb termination (how?)
-- more intrinsics are needed to, e.g., create an empty vector

type Terminate = ()
terminate :: Terminate
terminate = ()

--------------------------------------

class Ret a r where
    ret :: a -> CodeGenFunction r Terminate

instance (IsFirstClass a, IsConst a) => Ret a a where
    ret a = ret (valueOf a)

instance Ret (Value a) a where
    ret (Value a) = do
        withCurrentBuilder $ \ bldPtr -> FFI.buildRet bldPtr a
        return terminate

instance Ret () () where
    ret _ = do
        withCurrentBuilder $ FFI.buildRetVoid
        return terminate

--------------------------------------

condBr :: Value Bool -> BasicBlock -> BasicBlock -> CodeGenFunction r Terminate
condBr (Value b) (BasicBlock t1) (BasicBlock t2) = do
    withCurrentBuilder $ \ bldPtr -> FFI.buildCondBr bldPtr b t1 t2
    return terminate

--------------------------------------

br :: BasicBlock -> CodeGenFunction r Terminate
br (BasicBlock t) = do
    withCurrentBuilder $ \ bldPtr -> FFI.buildBr bldPtr t
    return terminate

--------------------------------------

switch :: (IsInteger a) =>
          Value a -> BasicBlock -> [(ConstValue a, BasicBlock)] -> CodeGenFunction r Terminate
switch (Value val) (BasicBlock dflt) arms = do
    withCurrentBuilder $ \ bldPtr -> do
        inst <- FFI.buildSwitch bldPtr val dflt (fromIntegral $ length arms)
        sequence_ [ FFI.addCase inst c b | (ConstValue c, BasicBlock b) <- arms ]
    return terminate

--------------------------------------

unwind :: CodeGenFunction r Terminate
unwind = do
    withCurrentBuilder FFI.buildUnwind
    return terminate

unreachable :: CodeGenFunction r Terminate
unreachable = do
    withCurrentBuilder FFI.buildUnreachable
    return terminate

--------------------------------------

-- XXX Vector ops not implemented

type FFIBinOp = FFI.BuilderRef -> FFI.ValueRef -> FFI.ValueRef -> U.CString -> IO FFI.ValueRef
type FFIConstBinOp = FFI.ValueRef -> FFI.ValueRef -> FFI.ValueRef

class ABinOp a b c | a b -> c where
    abinop :: FFIConstBinOp -> FFIBinOp -> a -> b -> CodeGenFunction r c

add, sub, mul :: (IsArithmetic c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
add = abinop FFI.constAdd FFI.buildAdd
sub = abinop FFI.constSub FFI.buildSub
mul = abinop FFI.constMul FFI.buildMul

udiv, sdiv, urem, srem :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
udiv = abinop FFI.constUDiv FFI.buildUDiv
sdiv = abinop FFI.constSDiv FFI.buildSDiv
urem = abinop FFI.constURem FFI.buildURem
srem = abinop FFI.constSRem FFI.buildSRem

fdiv, frem :: (IsFloating c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
fdiv = abinop FFI.constFDiv FFI.buildFDiv
frem = abinop FFI.constFRem FFI.buildFRem

shl, lshr, ashr, and, or, xor :: (IsInteger c, ABinOp a b (v c)) =>
     	   	      	         a -> b -> CodeGenFunction r (v c)
shl  = abinop FFI.constShl  FFI.buildShl
lshr = abinop FFI.constLShr FFI.buildLShr
ashr = abinop FFI.constAShr FFI.buildAShr
and  = abinop FFI.constAnd  FFI.buildAnd
or   = abinop FFI.constOr   FFI.buildOr
xor  = abinop FFI.constXor  FFI.buildXor

instance ABinOp (Value a) (Value a) (Value a) where
    abinop _ op (Value a1) (Value a2) = buildBinOp op a1 a2

instance ABinOp (ConstValue a) (Value a) (Value a) where
    abinop _ op (ConstValue a1) (Value a2) = buildBinOp op a1 a2

instance ABinOp (Value a) (ConstValue a) (Value a) where
    abinop _ op (Value a1) (ConstValue a2) = buildBinOp op a1 a2

instance ABinOp (ConstValue a) (ConstValue a) (ConstValue a) where
    abinop cop _ (ConstValue a1) (ConstValue a2) =
        return $ ConstValue $ cop a1 a2

instance (IsConst a) => ABinOp (Value a) a (Value a) where
    abinop cop op a1 a2 = abinop cop op a1 (constOf a2)

instance (IsConst a) => ABinOp a (Value a) (Value a) where
    abinop cop op a1 a2 = abinop cop op (constOf a1) a2

--instance (IsConst a) => ABinOp a a (ConstValue a) where
--    abinop cop op a1 a2 = abinop cop op (constOf a1) (constOf a2)

buildBinOp :: FFIBinOp -> FFI.ValueRef -> FFI.ValueRef -> CodeGenFunction r (Value a)
buildBinOp op a1 a2 =
    liftM Value $
    withCurrentBuilder $ \ bld ->
      U.withEmptyCString $ op bld a1 a2

--------------------------------------

extractelement :: Value (Vector n a) -> Value Word32 -> CodeGenFunction r (Value a)
extractelement (Value vec) (Value i) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildExtractElement bldPtr vec i

insertelement :: Value (Vector n a) -> Value a -> Value Word32 ->
                 CodeGenFunction r (Value (Vector n a))
insertelement (Value vec) (Value e) (Value i) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildInsertElement bldPtr vec e i

shufflevector :: Value (Vector n a) -> Value (Vector n a) ->
                 ConstValue (Vector n Word32) -> CodeGenFunction r (Value (Vector n a))
shufflevector (Value a) (Value b) (ConstValue mask) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildShuffleVector bldPtr a b mask


--------------------------------------

-- XXX should allows constants

-- XXX size a > size b not enforced
trunc :: (IsInteger a, IsInteger b) => Value a -> CodeGenFunction r (Value b)
trunc = convert FFI.buildTrunc

-- XXX size a < size b not enforced
zext :: (IsInteger a, IsInteger b) => Value a -> CodeGenFunction r (Value b)
zext = convert FFI.buildZExt

-- XXX size a < size b not enforced
sext :: (IsInteger a, IsInteger b) => Value a -> CodeGenFunction r (Value b)
sext = convert FFI.buildSExt

-- XXX size a > size b not enforced
fptrunc :: (IsFloating a, IsFloating b) => Value a -> CodeGenFunction r (Value b)
fptrunc = convert FFI.buildFPTrunc

-- XXX size a < size b not enforced
fpext :: (IsFloating a, IsFloating b) => Value a -> CodeGenFunction r (Value b)
fpext = convert FFI.buildFPExt

fptoui :: (IsFloating a, IsInteger b) => Value a -> CodeGenFunction r (Value b)
fptoui = convert FFI.buildFPToUI

fptosi :: (IsFloating a, IsInteger b) => Value a -> CodeGenFunction r (Value b)
fptosi = convert FFI.buildFPToSI

uitofp :: (IsInteger a, IsFloating b) => Value a -> CodeGenFunction r (Value b)
uitofp = convert FFI.buildUIToFP

sitofp :: (IsInteger a, IsFloating b) => Value a -> CodeGenFunction r (Value b)
sitofp = convert FFI.buildSIToFP

ptrtoint :: (IsInteger b) => Value (Ptr a) -> CodeGenFunction r (Value b)
ptrtoint = convert FFI.buildPtrToInt

inttoptr :: (IsInteger a, IsType b) => Value (Ptr a) -> CodeGenFunction r (Value (Ptr b))
inttoptr = convert FFI.buildIntToPtr

-- XXX a and b must use the same space, and there are also pointer restrictions
bitcast :: (IsFirstClass a, IsFirstClass b) => Value a -> CodeGenFunction r (Value b)
bitcast = convert FFI.buildBitCast

type FFIConvert = FFI.BuilderRef -> FFI.ValueRef -> FFI.TypeRef -> U.CString -> IO FFI.ValueRef

convert :: forall a b r . (IsType b) => FFIConvert -> Value a -> CodeGenFunction r (Value b)
convert conv (Value a) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ conv bldPtr a (typeRef (undefined :: b))

--------------------------------------

data IntPredicate =
    IntEQ                       -- ^ equal
  | IntNE                       -- ^ not equal
  | IntUGT                      -- ^ unsigned greater than
  | IntUGE                      -- ^ unsigned greater or equal
  | IntULT                      -- ^ unsigned less than
  | IntULE                      -- ^ unsigned less or equal
  | IntSGT                      -- ^ signed greater than
  | IntSGE                      -- ^ signed greater or equal
  | IntSLT                      -- ^ signed less than
  | IntSLE                      -- ^ signed less or equal
    deriving (Eq, Ord, Enum, Show)

data RealPredicate =
    RealFalse           -- ^ Always false (always folded)
  | RealOEQ             -- ^ True if ordered and equal
  | RealOGT             -- ^ True if ordered and greater than
  | RealOGE             -- ^ True if ordered and greater than or equal
  | RealOLT             -- ^ True if ordered and less than
  | RealOLE             -- ^ True if ordered and less than or equal
  | RealONE             -- ^ True if ordered and operands are unequal
  | RealORD             -- ^ True if ordered (no nans)
  | RealUNO             -- ^ True if unordered: isnan(X) | isnan(Y)
  | RealUEQ             -- ^ True if unordered or equal
  | RealUGT             -- ^ True if unordered or greater than
  | RealUGE             -- ^ True if unordered, greater than, or equal
  | RealULT             -- ^ True if unordered or less than
  | RealULE             -- ^ True if unordered, less than, or equal
  | RealUNE             -- ^ True if unordered or not equal
  | RealT               -- ^ Always true (always folded)
    deriving (Eq, Ord, Enum, Show)

class CmpOp a b c | a b -> c where
    cmpop :: FFIBinOp -> a -> b -> CodeGenFunction r (Value Bool)

instance CmpOp (Value a) (Value a) a where
    cmpop op (Value a1) (Value a2) = buildBinOp op a1 a2

instance (IsConst a) => CmpOp a (Value a) a where
    cmpop op a1 a2 = cmpop op (valueOf a1) a2

instance (IsConst a) => CmpOp (Value a) a a where
    cmpop op a1 a2 = cmpop op a1 (valueOf a2)

icmp :: (IsInteger c, CmpOp a b c) =>
        IntPredicate -> a -> b -> CodeGenFunction r (Value Bool)
icmp p = cmpop (flip FFI.buildICmp (fromIntegral (fromEnum p + 32)))

fcmp :: (IsFloating c, CmpOp a b c) =>
        RealPredicate -> a -> b -> CodeGenFunction r (Value Bool)
fcmp p = cmpop (flip FFI.buildFCmp (fromIntegral (fromEnum p)))

--------------------------------------

type Caller = FFI.BuilderRef -> [FFI.ValueRef] -> IO FFI.ValueRef

class CallArgs f g | f -> g, g -> f where
    doCall :: Caller -> [FFI.ValueRef] -> g

instance (CallArgs b b') => CallArgs (a -> b) (Value a -> b') where
    doCall mkCall args = \ (Value arg) -> doCall mkCall (arg : args)

instance CallArgs (IO a) (CodeGenFunction r (Value a)) where doCall = doCallDef
{-
instance CallArgs Float        (CodeGenFunction r (Value Float))        where doCall = doCallDef
instance CallArgs Double       (CodeGenFunction r (Value Double))       where doCall = doCallDef
instance CallArgs FP128        (CodeGenFunction r (Value FP128))        where doCall = doCallDef
instance (IsTypeNumber n) =>
         CallArgs (IntN n)     (CodeGenFunction r (Value (IntN n)))     where doCall = doCallDef
instance (IsTypeNumber n) =>
         CallArgs (WordN n)    (CodeGenFunction r (Value (WordN n)))    where doCall = doCallDef
instance CallArgs Bool         (CodeGenFunction r (Value Bool))         where doCall = doCallDef
instance CallArgs Int8         (CodeGenFunction r (Value Int8))         where doCall = doCallDef
instance CallArgs Int16        (CodeGenFunction r (Value Int16))        where doCall = doCallDef
instance CallArgs Int32        (CodeGenFunction r (Value Int32))        where doCall = doCallDef
instance CallArgs Int64        (CodeGenFunction r (Value Int64))        where doCall = doCallDef
instance CallArgs Word8        (CodeGenFunction r (Value Word8))        where doCall = doCallDef
instance CallArgs Word16       (CodeGenFunction r (Value Word16))       where doCall = doCallDef
instance CallArgs Word32       (CodeGenFunction r (Value Word32))       where doCall = doCallDef
instance CallArgs Word64       (CodeGenFunction r (Value Word64))       where doCall = doCallDef
instance (IsTypeNumber n, IsPrimitive a) =>
         CallArgs (Vector n a) (CodeGenFunction r (Value (Vector n a))) where doCall = doCallDef
instance (IsType a) =>
         CallArgs (Ptr a)      (CodeGenFunction r (Value (Ptr a)))      where doCall = doCallDef
-}

doCallDef :: Caller -> [FFI.ValueRef] -> CodeGenFunction r (Value a)
doCallDef mkCall args =
    withCurrentBuilder $ \ bld -> 
      liftM Value $ mkCall bld (reverse args)

call :: (CallArgs f g) => Function f -> g
call (Value f) = doCall (U.makeCall f) []

invoke :: (CallArgs f g) => BasicBlock -> BasicBlock -> Function f -> g
invoke (BasicBlock norm) (BasicBlock expt) (Value f) =
    doCall (U.makeInvoke norm expt f) []

--------------------------------------

-- XXX could do const song and dance
phi :: forall a r . (IsFirstClass a) => [(Value a, BasicBlock)] -> CodeGenFunction r (Value a)
phi incoming = 
    liftM Value $
      withCurrentBuilder $ \ bldPtr ->
        U.buildPhi bldPtr (typeRef (undefined :: a))
                   [ (v, b) | (Value v, BasicBlock b) <- incoming ]

--------------------------------------

-- XXX could do const song and dance
select :: (IsFirstClass a) => Value Bool -> Value a -> Value a -> CodeGenFunction r (Value a)
select (Value cnd) (Value thn) (Value els) =
    liftM Value $
      withCurrentBuilder $ \ bldPtr ->
        U.withEmptyCString $
          FFI.buildSelect bldPtr cnd thn els

--------------------------------------

-- XXX What's the type returned by malloc
malloc :: forall a r . (IsSized a) => CodeGenFunction r (Value (Ptr a))
malloc =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildMalloc bldPtr (typeRef (undefined :: a))

-- XXX What's the type returned by arrayMalloc?
arrayMalloc :: forall a r . (IsSized a) =>
               Value (Word32) -> CodeGenFunction r (Value (Ptr a)) -- XXX
arrayMalloc (Value n) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $
        FFI.buildArrayMalloc bldPtr (typeRef (undefined :: a)) n

-- XXX What's the type returned by malloc
alloca :: forall a n r . (IsSized a) => CodeGenFunction r (Value (Ptr a))
alloca =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildAlloca bldPtr (typeRef (undefined :: a))

-- XXX What's the type returned by arrayAlloca?
arrayAlloca :: forall a n r . (IsSized a) =>
               Value (Word32) -> CodeGenFunction r (Value (Ptr a))
arrayAlloca (Value n) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $
        FFI.buildArrayAlloca bldPtr (typeRef (undefined :: a)) n

-- XXX What's the type of free?
free :: Value (Ptr a) -> CodeGenFunction r (Value ())
free (Value a) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr -> FFI.buildFree bldPtr a

load :: Value (Ptr a) -> CodeGenFunction r (Value a)
load (Value p) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildLoad bldPtr p

store :: Value (Ptr a) -> Value a -> CodeGenFunction r (Value ())
store (Value v) (Value p) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      FFI.buildStore bldPtr v p

-- XXX type is wrong
getElementPtr :: (IsInteger i) =>
                 Value (Ptr a) -> [Value i] -> CodeGenFunction r (Value (Ptr b))
getElementPtr (Value ptr) ixs =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withArrayLen [ v | Value v <- ixs ] $ \ idxLen idxPtr ->
        U.withEmptyCString $
          FFI.buildGEP bldPtr ptr idxPtr (fromIntegral idxLen)
