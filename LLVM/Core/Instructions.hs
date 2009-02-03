{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, TypeSynonymInstances, ScopedTypeVariables, OverlappingInstances, FlexibleContexts, TypeOperators #-}
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
    -- | Arithmetic operations with the normal semantics.
    -- The u instractions are unsigned, the s instructions are signed.
    add, sub, mul, neg,
    udiv, sdiv, fdiv, urem, srem, frem,
    -- * Logical binary operations
    -- |Logical instructions with the normal semantics.
    shl, lshr, ashr, and, or, xor, inv,
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
    -- * Comparison
    IntPredicate(..), FPPredicate(..),
    CmpRet,
    icmp, fcmp,
    select,
    -- * Other
    phi, addPhiInputs,
    call,
    -- * Classes and types
    Terminate,
    Ret, CallArgs, ABinOp, CmpOp, FunctionArgs, FunctionRet, IsConst,
    AllocArg,
    GetElementPtr, IsIndexArg
    ) where
import Prelude hiding (and, or)
import Control.Monad(liftM)
import Data.Int
import Data.Word
import Foreign.C(CInt)
import Data.TypeLevel((:<:), (:>:), (:==:))
import qualified LLVM.FFI.Core as FFI
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

-- |Acceptable arguments to the 'ret' instruction.
class Ret a r where
    ret' :: a -> CodeGenFunction r Terminate

-- | Return from the current function with the given value.  Use () as the return value for what would be a void function is C.
ret :: (Ret a r) => a -> CodeGenFunction r Terminate
ret = ret'

instance (IsFirstClass a, IsConst a) => Ret a a where
    ret' = ret . valueOf

instance Ret (Value a) a where
    ret' (Value a) = do
        withCurrentBuilder $ \ bldPtr -> FFI.buildRet bldPtr a
        return terminate

instance Ret () () where
    ret' _ = do
        withCurrentBuilder $ FFI.buildRetVoid
        return terminate

--------------------------------------

-- | Branch to the first basic block if the boolean is true, otherwise to the second basic block.
condBr :: Value Bool -- ^ Boolean to branch upon.
       -> BasicBlock -- ^ Target for true.
       -> BasicBlock -- ^ Target for false.
       -> CodeGenFunction r Terminate
condBr (Value b) (BasicBlock t1) (BasicBlock t2) = do
    withCurrentBuilder $ \ bldPtr -> FFI.buildCondBr bldPtr b t1 t2
    return terminate

--------------------------------------

-- | Unconditionally branch to the given basic block.
br :: BasicBlock  -- ^ Branch target.
   -> CodeGenFunction r Terminate
br (BasicBlock t) = do
    withCurrentBuilder $ \ bldPtr -> FFI.buildBr bldPtr t
    return terminate

--------------------------------------

-- | Branch table instruction.
switch :: (IsInteger a)
       => Value a                        -- ^ Value to branch upon.
       -> BasicBlock                     -- ^ Default branch target.
       -> [(ConstValue a, BasicBlock)]   -- ^ Labels and corresponding branch targets.
       -> CodeGenFunction r Terminate
switch (Value val) (BasicBlock dflt) arms = do
    withCurrentBuilder $ \ bldPtr -> do
        inst <- FFI.buildSwitch bldPtr val dflt (fromIntegral $ length arms)
        sequence_ [ FFI.addCase inst c b | (ConstValue c, BasicBlock b) <- arms ]
    return terminate

--------------------------------------

-- |Unwind the call stack until a function call performed with 'invoke' is reached.
-- I.e., throw a non-local exception.
unwind :: CodeGenFunction r Terminate
unwind = do
    withCurrentBuilder FFI.buildUnwind
    return terminate

-- |Inform the code generator that this code can never be reached.
unreachable :: CodeGenFunction r Terminate
unreachable = do
    withCurrentBuilder FFI.buildUnreachable
    return terminate

--------------------------------------

type FFIBinOp = FFI.BuilderRef -> FFI.ValueRef -> FFI.ValueRef -> U.CString -> IO FFI.ValueRef
type FFIConstBinOp = FFI.ValueRef -> FFI.ValueRef -> FFI.ValueRef

-- |Acceptable arguments to arithmetic binary instructions.
class ABinOp a b c | a b -> c where
    abinop :: FFIConstBinOp -> FFIBinOp -> a -> b -> CodeGenFunction r c

add :: (IsArithmetic c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
add = abinop FFI.constAdd FFI.buildAdd
sub :: (IsArithmetic c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
sub = abinop FFI.constSub FFI.buildSub
mul :: (IsArithmetic c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
mul = abinop FFI.constMul FFI.buildMul

udiv :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
udiv = abinop FFI.constUDiv FFI.buildUDiv
sdiv :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
sdiv = abinop FFI.constSDiv FFI.buildSDiv
urem :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
urem = abinop FFI.constURem FFI.buildURem
srem :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
srem = abinop FFI.constSRem FFI.buildSRem

-- | Floating point division.
fdiv :: (IsFloating c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
fdiv = abinop FFI.constFDiv FFI.buildFDiv
-- | Floating point remainder.
frem :: (IsFloating c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
frem = abinop FFI.constFRem FFI.buildFRem

shl :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
shl  = abinop FFI.constShl  FFI.buildShl
lshr :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
lshr = abinop FFI.constLShr FFI.buildLShr
ashr :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
ashr = abinop FFI.constAShr FFI.buildAShr
and :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
and  = abinop FFI.constAnd  FFI.buildAnd
or :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
or   = abinop FFI.constOr   FFI.buildOr
xor :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
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

type FFIUnOp = FFI.BuilderRef -> FFI.ValueRef -> U.CString -> IO FFI.ValueRef

buildUnOp :: FFIUnOp -> FFI.ValueRef -> CodeGenFunction r (Value a)
buildUnOp op a =
    liftM Value $
    withCurrentBuilder $ \ bld ->
      U.withEmptyCString $ op bld a

neg :: (IsArithmetic a) => Value a -> CodeGenFunction r (Value a)
neg (Value x) = buildUnOp FFI.buildNeg x

inv :: (IsInteger a) => Value a -> CodeGenFunction r (Value a)
inv (Value x) = buildUnOp FFI.buildNot x

--------------------------------------

-- | Get a value from a vector.
extractelement :: Value (Vector n a)               -- ^ Vector
               -> Value Word32                     -- ^ Index into the vector
               -> CodeGenFunction r (Value a)
extractelement (Value vec) (Value i) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildExtractElement bldPtr vec i

-- | Insert a value into a vector, nondescructive.
insertelement :: Value (Vector n a)                -- ^ Vector
              -> Value a                           -- ^ Value to insert
              -> Value Word32                      -- ^ Index into the vector
              -> CodeGenFunction r (Value (Vector n a))
insertelement (Value vec) (Value e) (Value i) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildInsertElement bldPtr vec e i

-- XXX The documentation say the mask and result can  different length from
-- the two first operand, but the C++ code doesn't do that.
-- | Permute vector.
shufflevector :: Value (Vector n a)
              -> Value (Vector n a)
              -> ConstValue (Vector n Word32)
              -> CodeGenFunction r (Value (Vector n a))
shufflevector (Value a) (Value b) (ConstValue mask) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildShuffleVector bldPtr a b mask


--------------------------------------

-- XXX should allows constants

-- | Truncate a value to a shorter bit width.
trunc :: (IsInteger a, IsInteger b, IsPrimitive a, IsPrimitive b, IsSized a sa, IsSized b sb, sa :>: sb)
      => Value a -> CodeGenFunction r (Value b)
trunc = convert FFI.buildTrunc

-- | Zero extend a value to a wider width.
zext :: (IsInteger a, IsInteger b, IsPrimitive a, IsPrimitive b, IsSized a sa, IsSized b sb, sa :<: sb)
     => Value a -> CodeGenFunction r (Value b)
zext = convert FFI.buildZExt

-- | Sign extend a value to wider width.
sext :: (IsInteger a, IsInteger b, IsPrimitive a, IsPrimitive b, IsSized a sa, IsSized b sb, sa :<: sb)
     => Value a -> CodeGenFunction r (Value b)
sext = convert FFI.buildSExt

-- | Truncate a floating point value.
fptrunc :: (IsFloating a, IsFloating b, IsPrimitive a, IsPrimitive b, IsSized a sa, IsSized b sb, sa :>: sb)
        => Value a -> CodeGenFunction r (Value b)
fptrunc = convert FFI.buildFPTrunc

-- | Extend a floating point value.
fpext :: (IsFloating a, IsFloating b, IsPrimitive a, IsPrimitive b, IsSized a sa, IsSized b sb, sa :<: sb)
      => Value a -> CodeGenFunction r (Value b)
fpext = convert FFI.buildFPExt

-- XXX The fp<->i conversion can handle vectors.
-- | Convert a floating point value to an unsigned integer.
fptoui :: (IsFloating a, IsInteger b, IsPrimitive a, IsPrimitive b) => Value a -> CodeGenFunction r (Value b)
fptoui = convert FFI.buildFPToUI

-- | Convert a floating point value to a signed integer.
fptosi :: (IsFloating a, IsInteger b, IsPrimitive a, IsPrimitive b) => Value a -> CodeGenFunction r (Value b)
fptosi = convert FFI.buildFPToSI

-- | Convert an unsigned integer to a floating point value.
uitofp :: (IsInteger a, IsFloating b, IsPrimitive a, IsPrimitive b) => Value a -> CodeGenFunction r (Value b)
uitofp = convert FFI.buildUIToFP

-- | Convert a signed integer to a floating point value.
sitofp :: (IsInteger a, IsFloating b, IsPrimitive a, IsPrimitive b) => Value a -> CodeGenFunction r (Value b)
sitofp = convert FFI.buildSIToFP

-- | Convert a pointer to an integer.
ptrtoint :: (IsInteger b, IsPrimitive b) => Value (Ptr a) -> CodeGenFunction r (Value b)
ptrtoint = convert FFI.buildPtrToInt

-- | Convert an integer to a pointer.
inttoptr :: (IsInteger a, IsType b) => Value (Ptr a) -> CodeGenFunction r (Value (Ptr b))
inttoptr = convert FFI.buildIntToPtr

-- | Convert between to values of the same size by just copying the bit pattern.
bitcast :: (IsFirstClass a, IsFirstClass b, IsSized a sa, IsSized b sb, sa :==: sb)
        => Value a -> CodeGenFunction r (Value b)
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

fromIntPredicate :: IntPredicate -> CInt
fromIntPredicate p = fromIntegral (fromEnum p + 32)

data FPPredicate =
    FPFalse           -- ^ Always false (always folded)
  | FPOEQ             -- ^ True if ordered and equal
  | FPOGT             -- ^ True if ordered and greater than
  | FPOGE             -- ^ True if ordered and greater than or equal
  | FPOLT             -- ^ True if ordered and less than
  | FPOLE             -- ^ True if ordered and less than or equal
  | FPONE             -- ^ True if ordered and operands are unequal
  | FPORD             -- ^ True if ordered (no nans)
  | FPUNO             -- ^ True if unordered: isnan(X) | isnan(Y)
  | FPUEQ             -- ^ True if unordered or equal
  | FPUGT             -- ^ True if unordered or greater than
  | FPUGE             -- ^ True if unordered, greater than, or equal
  | FPULT             -- ^ True if unordered or less than
  | FPULE             -- ^ True if unordered, less than, or equal
  | FPUNE             -- ^ True if unordered or not equal
  | FPT               -- ^ Always true (always folded)
    deriving (Eq, Ord, Enum, Show)

fromFPPredicate :: FPPredicate -> CInt
fromFPPredicate p = fromIntegral (fromEnum p)

-- |Acceptable operands to comparison instructions.
class CmpOp a b c d | a b -> c where
    cmpop :: FFIBinOp -> a -> b -> CodeGenFunction r (Value d)

instance CmpOp (Value a) (Value a) a d where
    cmpop op (Value a1) (Value a2) = buildBinOp op a1 a2

instance (IsConst a) => CmpOp a (Value a) a d where
    cmpop op a1 a2 = cmpop op (valueOf a1) a2

instance (IsConst a) => CmpOp (Value a) a a d where
    cmpop op a1 a2 = cmpop op a1 (valueOf a2)

class CmpRet a b | a -> b
instance CmpRet Float Bool
instance CmpRet Double Bool
instance CmpRet FP128 Bool
instance CmpRet Bool Bool
instance CmpRet Word8 Bool
instance CmpRet Word16 Bool
instance CmpRet Word32 Bool
instance CmpRet Word64 Bool
instance CmpRet Int8 Bool
instance CmpRet Int16 Bool
instance CmpRet Int32 Bool
instance CmpRet Int64 Bool
instance CmpRet (Vector n a) (Vector n Bool)

-- | Compare integers.
icmp :: (IsInteger c, CmpOp a b c d, CmpRet c d) =>
        IntPredicate -> a -> b -> CodeGenFunction r (Value d)
icmp p = cmpop (flip FFI.buildICmp (fromIntPredicate p))

-- | Compare floating point values.
fcmp :: (IsFloating c, CmpOp a b c d, CmpRet c d) =>
        FPPredicate -> a -> b -> CodeGenFunction r (Value d)
fcmp p = cmpop (flip FFI.buildFCmp (fromFPPredicate p))

--------------------------------------

-- XXX could do const song and dance
-- | Select between two values depending on a boolean.
select :: (IsFirstClass a, CmpRet a b) => Value b -> Value a -> Value a -> CodeGenFunction r (Value a)
select (Value cnd) (Value thn) (Value els) =
    liftM Value $
      withCurrentBuilder $ \ bldPtr ->
        U.withEmptyCString $
          FFI.buildSelect bldPtr cnd thn els

--------------------------------------

type Caller = FFI.BuilderRef -> [FFI.ValueRef] -> IO FFI.ValueRef

-- |Acceptable arguments to 'call'.
class CallArgs f g | f -> g, g -> f where
    doCall :: Caller -> [FFI.ValueRef] -> f -> g

instance (CallArgs b b') => CallArgs (a -> b) (Value a -> b') where
    doCall mkCall args f (Value arg) = doCall mkCall (arg : args) (f (undefined :: a))

--instance (CallArgs b b') => CallArgs (a -> b) (ConstValue a -> b') where
--    doCall mkCall args f (ConstValue arg) = doCall mkCall (arg : args) (f (undefined :: a))

instance CallArgs (IO a) (CodeGenFunction r (Value a)) where
    doCall = doCallDef

doCallDef :: Caller -> [FFI.ValueRef] -> b -> CodeGenFunction r (Value a)
doCallDef mkCall args _ =
    withCurrentBuilder $ \ bld -> 
      liftM Value $ mkCall bld (reverse args)

-- | Call a function with the given arguments.  The 'call' instruction is variadic, i.e., the number of arguments
-- it takes depends on the type of /f/.
call :: (CallArgs f g) => Function f -> g
call (Value f) = doCall (U.makeCall f) [] (undefined :: f)

-- | Call a function with exception handling.
invoke :: (CallArgs f g)
       => BasicBlock         -- ^Normal return point.
       -> BasicBlock         -- ^Exception return point.
       -> Function f         -- ^Function to call.
       -> g
invoke (BasicBlock norm) (BasicBlock expt) (Value f) =
    doCall (U.makeInvoke norm expt f) [] (undefined :: f)

--------------------------------------

-- XXX could do const song and dance
-- |Join several variables (virtual registers) from different basic blocks into one.
-- All of the variables in the list are joined.  See also 'addPhiInputs'.
phi :: forall a r . (IsFirstClass a) => [(Value a, BasicBlock)] -> CodeGenFunction r (Value a)
phi incoming = 
    liftM Value $
      withCurrentBuilder $ \ bldPtr -> do
        inst <- U.buildEmptyPhi bldPtr (typeRef (undefined :: a))
        U.addPhiIns inst [ (v, b) | (Value v, BasicBlock b) <- incoming ]
        return inst

-- |Add additional inputs to an existing phi node.
-- The reason for this instruction is that sometimes the structure of the code
-- makes it impossible to have all variables in scope at the point where you need the phi node.
addPhiInputs :: forall a r . (IsFirstClass a)
             => Value a                      -- ^Must be a variable from a call to 'phi'.
             -> [(Value a, BasicBlock)]      -- ^Variables to add.
             -> CodeGenFunction r ()
addPhiInputs (Value inst) incoming =
    liftIO $ U.addPhiIns inst [ (v, b) | (Value v, BasicBlock b) <- incoming ]
    

--------------------------------------

-- | Acceptable argument to array memory allocation.
class AllocArg a where
    getAllocArg :: a -> FFI.ValueRef
instance AllocArg (Value Word32) where
    getAllocArg (Value v) = v
instance AllocArg (ConstValue Word32) where
    getAllocArg = unConst
instance AllocArg Word32 where
    getAllocArg = unConst . constOf

-- XXX What's the type returned by malloc
-- | Allocate heap memory.
malloc :: forall a r s . (IsSized a s) => CodeGenFunction r (Value (Ptr a))
malloc =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildMalloc bldPtr (typeRef (undefined :: a))

-- XXX What's the type returned by arrayMalloc?
-- | Allocate heap (array) memory.
arrayMalloc :: forall a n r s . (IsSized a n, AllocArg s) =>
               s -> CodeGenFunction r (Value (Ptr a)) -- XXX
arrayMalloc s =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $
        FFI.buildArrayMalloc bldPtr (typeRef (undefined :: a)) (getAllocArg s)

-- XXX What's the type returned by malloc
-- | Allocate stack memory.
alloca :: forall a r s . (IsSized a s) => CodeGenFunction r (Value (Ptr a))
alloca =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildAlloca bldPtr (typeRef (undefined :: a))

-- XXX What's the type returned by arrayAlloca?
-- | Allocate stack (array) memory.
arrayAlloca :: forall a n r s . (IsSized a n, AllocArg s) =>
               s -> CodeGenFunction r (Value (Ptr a))
arrayAlloca s =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $
        FFI.buildArrayAlloca bldPtr (typeRef (undefined :: a)) (getAllocArg s)

-- XXX What's the type of free?
-- | Free heap memory.
free :: Value (Ptr a) -> CodeGenFunction r (Value ())
free (Value a) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr -> FFI.buildFree bldPtr a

-- | Load a value from memory.
load :: Value (Ptr a)                   -- ^ Address to load from.
     -> CodeGenFunction r (Value a)
load (Value p) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildLoad bldPtr p

-- | Store a value in memory
store :: Value a                        -- ^ Value to store.
      -> Value (Ptr a)                  -- ^ Address to store to.
      -> CodeGenFunction r (Value ())
store (Value v) (Value p) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      FFI.buildStore bldPtr v p

{-
-- XXX type is wrong
-- | Address arithmetic.  See LLVM description.
-- (The type isn't as accurate as it should be.)
getElementPtr :: (IsInteger i) =>
                 Value (Ptr a) -> [Value i] -> CodeGenFunction r (Value (Ptr b))
getElementPtr (Value ptr) ixs =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withArrayLen [ v | Value v <- ixs ] $ \ idxLen idxPtr ->
        U.withEmptyCString $
          FFI.buildGEP bldPtr ptr idxPtr (fromIntegral idxLen)
-}

-- |Acceptable arguments to 'getElementPointer'.
class GetElementPtr optr ixs nptr | optr ixs -> nptr {-, ixs nptr -> optr, nptr optr -> ixs-} where
    getIxList :: optr -> ixs -> [FFI.ValueRef]

-- |Acceptable single index to 'getElementPointer'.
class IsIndexArg a where
    getArg :: a -> FFI.ValueRef

instance IsIndexArg (Value Word32) where
    getArg (Value v) = v

instance IsIndexArg (Value Word64) where
    getArg (Value v) = v

instance IsIndexArg (Value Int32) where
    getArg (Value v) = v

instance IsIndexArg (Value Int64) where
    getArg (Value v) = v

instance IsIndexArg (ConstValue Word32) where
    getArg = unConst

instance IsIndexArg (ConstValue Word64) where
    getArg = unConst

instance IsIndexArg (ConstValue Int32) where
    getArg = unConst

instance IsIndexArg (ConstValue Int64) where
    getArg = unConst

instance IsIndexArg Word32 where
    getArg = unConst . constOf

instance IsIndexArg Word64 where
    getArg = unConst . constOf

instance IsIndexArg Int32 where
    getArg = unConst . constOf

instance IsIndexArg Int64 where
    getArg = unConst . constOf

unConst :: ConstValue a -> FFI.ValueRef
unConst (ConstValue v) = v

-- End of indexing
instance GetElementPtr a () a where
    getIxList _ () = []

-- Index in Array
instance (GetElementPtr o i n, IsIndexArg a) => GetElementPtr (Array k o) (a, i) n where
    getIxList _ (v, i) = getArg v : getIxList (undefined :: o) i

-- Index in Vector
instance (GetElementPtr o i n, IsIndexArg a) => GetElementPtr (Vector k o) (a, i) n where
    getIxList _ (v, i) = getArg v : getIxList (undefined :: o) i

-- | Address arithmetic.  See LLVM description.
-- The index is a nested tuple of the form @(i1,(i2,( ... ())))@.
-- (This is without a doubt the most confusing LLVM instruction, but the types help.)
getElementPtr :: forall a o i n r . (GetElementPtr o i n, IsIndexArg a) =>
                 Value (Ptr o) -> (a, i) -> CodeGenFunction r (Value (Ptr n))
getElementPtr (Value ptr) (a, ixs) =
    let ixl = getArg a : getIxList (undefined :: o) ixs in
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withArrayLen ixl $ \ idxLen idxPtr ->
        U.withEmptyCString $
          FFI.buildGEP bldPtr ptr idxPtr (fromIntegral idxLen)


--------------------------------------
{-
instance (IsConst a) => Show (ConstValue a) -- XXX
instance (IsConst a) => Eq (ConstValue a)

{-
instance (IsConst a) => Eq (ConstValue a) where
    ConstValue x == ConstValue y  =
        if isFloating x then ConstValue (FFI.constFCmp (fromFPPredicate  FPOEQ) x y)
                        else ConstValue (FFI.constICmp (fromIntPredicate IntEQ) x y)
    ConstValue x /= ConstValue y  =
        if isFloating x then ConstValue (FFI.constFCmp (fromFPPredicate  FPONE) x y)
                        else ConstValue (FFI.constICmp (fromIntPredicate IntNE) x y)

instance (IsConst a) => Ord (ConstValue a) where
    ConstValue x <  ConstValue y  =
        if isFloating x then ConstValue (FFI.constFCmp (fromFPPredicate  FPOLT) x y)
                        else ConstValue (FFI.constICmp (fromIntPredicate IntLT) x y)
    ConstValue x <= ConstValue y  =
        if isFloating x then ConstValue (FFI.constFCmp (fromFPPredicate  FPOLE) x y)
                        else ConstValue (FFI.constICmp (fromIntPredicate IntLE) x y)
    ConstValue x >  ConstValue y  =
        if isFloating x then ConstValue (FFI.constFCmp (fromFPPredicate  FPOGT) x y)
                        else ConstValue (FFI.constICmp (fromIntPredicate IntGT) x y)
    ConstValue x >= ConstValue y  =
        if isFloating x then ConstValue (FFI.constFCmp (fromFPPredicate  FPOGE) x y)
                        else ConstValue (FFI.constICmp (fromIntPredicate IntGE) x y)
-}

instance (Num a, IsConst a) => Num (ConstValue a) where
    ConstValue x + ConstValue y  =  ConstValue (FFI.constAdd x y)
    ConstValue x - ConstValue y  =  ConstValue (FFI.constSub x y)
    ConstValue x * ConstValue y  =  ConstValue (FFI.constMul x y)
    negate (ConstValue x)        =  ConstValue (FFI.constNeg x)
    fromInteger x                =  constOf (fromInteger x :: a)
-}
