{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, TypeSynonymInstances, ScopedTypeVariables, OverlappingInstances, FlexibleContexts, TypeOperators, DeriveDataTypeable, ForeignFunctionInterface #-}
module LLVM.Core.Instructions(
    -- * ADT representation of IR
    BinOpDesc(..), InstrDesc(..), ArgDesc(..), getInstrDesc,
    -- * Terminator instructions
    ret,
    condBr,
    br,
    switch,
    invoke, invokeWithConv,
    unwind,
    unreachable,
    -- * Arithmetic binary operations
    -- | Arithmetic operations with the normal semantics.
    -- The u instractions are unsigned, the s instructions are signed.
    add, sub, mul, neg,
    iadd, isub, imul, ineg,
    fadd, fsub, fmul, fneg,
    idiv, irem,
    udiv, sdiv, fdiv, urem, srem, frem,
    -- * Logical binary operations
    -- |Logical instructions with the normal semantics.
    shl, lshr, ashr, and, or, xor, inv,
    -- * Vector operations
    extractelement,
    insertelement,
    shufflevector,
    -- * Aggregate operation
    extractvalue,
    insertvalue,
    -- * Memory access
    malloc, arrayMalloc,
    alloca, arrayAlloca,
    free,
    load,
    store,
    getElementPtr, getElementPtr0,
    -- * Conversions
    trunc, zext, sext,
    fptrunc, fpext,
    fptoui, fptosi, fptoint,
    uitofp, sitofp, inttofp,
    ptrtoint, inttoptr,
    bitcast, bitcastUnify,
    -- * Comparison
    CmpPredicate(..), IntPredicate(..), FPPredicate(..),
    CmpRet,
    cmp, pcmp, icmp, fcmp,
    select,
    -- * Other
    phi, addPhiInputs,
    call, callWithConv,

    -- * Classes and types
    Terminate,
    Ret, CallArgs, ABinOp, CmpOp, FunctionArgs, FunctionRet, IsConst,
    AllocArg,
    GetElementPtr, IsIndexArg, GetValue
    ) where
import Prelude hiding (and, or)
import Data.Typeable
import Control.Monad(liftM)
import Data.Int
import Data.Word
import Data.Map(fromList, (!))
import Foreign.Ptr (FunPtr, )
import Foreign.C(CInt, CUInt)
import Data.TypeLevel((:<:), (:>:), (:==:), (:*),
          D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, d1, toNum, Succ)
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

data ArgDesc = AV String | AI Int | AL String | AE

instance Show ArgDesc where
    -- show (AV s) = "V_" ++ s
    -- show (AI i) = "I_" ++ show i
    -- show (AL l) = "L_" ++ l
    show (AV s) = s
    show (AI i) = show i
    show (AL l) = l
    show AE = "voidarg?"

data BinOpDesc = BOAdd | BOAddNuw | BOAddNsw | BOAddNuwNsw | BOFAdd
               | BOSub | BOSubNuw | BOSubNsw | BOSubNuwNsw | BOFSub
               | BOMul | BOMulNuw | BOMulNsw | BOMulNuwNsw | BOFMul
               | BOUDiv | BOSDiv | BOSDivExact | BOFDiv | BOURem | BOSRem | BOFRem
               | BOShL | BOLShR | BOAShR | BOAnd | BOOr | BOXor
    deriving Show

-- FIXME: complete definitions for unimplemented instructions
data InstrDesc =
    -- terminators
    IDRet TypeDesc ArgDesc | IDRetVoid
  | IDBrCond ArgDesc ArgDesc ArgDesc | IDBrUncond ArgDesc
  | IDSwitch [(ArgDesc, ArgDesc)]
  | IDIndirectBr
  | IDInvoke
  | IDUnwind
  | IDUnreachable
    -- binary operators (including bitwise)
  | IDBinOp BinOpDesc TypeDesc ArgDesc ArgDesc
    -- memory access and addressing
  | IDAlloca TypeDesc Int Int | IDLoad TypeDesc ArgDesc | IDStore TypeDesc ArgDesc ArgDesc
  | IDGetElementPtr TypeDesc [ArgDesc]
    -- conversion
  | IDTrunc TypeDesc TypeDesc ArgDesc | IDZExt TypeDesc TypeDesc ArgDesc
  | IDSExt TypeDesc TypeDesc ArgDesc | IDFPtoUI TypeDesc TypeDesc ArgDesc
  | IDFPtoSI TypeDesc TypeDesc ArgDesc | IDUItoFP TypeDesc TypeDesc ArgDesc
  | IDSItoFP TypeDesc TypeDesc ArgDesc
  | IDFPTrunc TypeDesc TypeDesc ArgDesc | IDFPExt TypeDesc TypeDesc ArgDesc
  | IDPtrToInt TypeDesc TypeDesc ArgDesc | IDIntToPtr TypeDesc TypeDesc ArgDesc
  | IDBitcast TypeDesc TypeDesc ArgDesc
    -- other
  | IDICmp IntPredicate ArgDesc ArgDesc | IDFCmp FPPredicate ArgDesc ArgDesc
  | IDPhi TypeDesc [(ArgDesc, ArgDesc)] | IDCall TypeDesc ArgDesc [ArgDesc]
  | IDSelect TypeDesc ArgDesc ArgDesc | IDUserOp1 | IDUserOp2 | IDVAArg
    -- vector operators
  | IDExtractElement | IDInsertElement | IDShuffleVector
    -- aggregate operators
  | IDExtractValue | IDInsertValue
    -- invalid
  | IDInvalidOp
    deriving Show

-- TODO: overflow support for binary operations (add/sub/mul)
getInstrDesc :: FFI.ValueRef -> IO (String, InstrDesc)
getInstrDesc v = do
    valueName <- U.getValueNameU v
    opcode <- FFI.instGetOpcode v
    t <- FFI.typeOf v >>= typeDesc2
    -- FIXME: sizeof() does not work for types!
    --tsize <- FFI.typeOf v -- >>= FFI.sizeOf -- >>= FFI.constIntGetZExtValue >>= return . fromIntegral
    tsize <- return 1
    os <- U.getOperands v >>= mapM getArgDesc
    os0 <- if length os > 0 then return $ os !! 0 else return AE
    os1 <- if length os > 1 then return $ os !! 1 else return AE
    t2 <- (if not (null os) && (opcode >= 30 || opcode <= 41)
            then U.getOperands v >>= return . snd . head >>= FFI.typeOf >>= typeDesc2
            else return TDVoid)
    p <- if opcode `elem` [42, 43] then FFI.cmpInstGetPredicate v else return 0
    let instr =
            (if opcode >= 8 && opcode <= 25 -- binary arithmetic
             then IDBinOp (getBinOp opcode) t os0 os1
             else if opcode >= 30 && opcode <= 41 -- conversion
                  then (getConvOp opcode) t2 t os0
                  else case opcode of
                         { 1 -> if null os then IDRetVoid else IDRet t os0;
                           2 -> if length os == 1 then IDBrUncond os0 else IDBrCond os0 (os !! 2) os1;
                           3 -> IDSwitch $ toPairs os;
                           -- TODO (can skip for now)
                           -- 4 -> IndirectBr ; 5 -> Invoke ;
                           6 -> IDUnwind; 7 -> IDUnreachable;
                           26 -> IDAlloca (getPtrType t) tsize (getImmInt os0);
                           27 -> IDLoad t os0; 28 -> IDStore t os0 os1;
                           29 -> IDGetElementPtr t os;
                           42 -> IDICmp (toIntPredicate p) os0 os1;
                           43 -> IDFCmp (toFPPredicate p) os0 os1;
                           44 -> IDPhi t $ toPairs os;
                           -- FIXME: getelementptr arguments are not handled
                           45 -> IDCall t (last os) (init os);
                           46 -> IDSelect t os0 os1;
                           -- TODO (can skip for now)
                           -- 47 -> UserOp1 ; 48 -> UserOp2 ; 49 -> VAArg ;
                           -- 50 -> ExtractElement ; 51 -> InsertElement ; 52 -> ShuffleVector ;
                           -- 53 -> ExtractValue ; 54 -> InsertValue ;
                           _ -> IDInvalidOp })
    return (valueName, instr)
    --if instr /= InvalidOp then return instr else fail $ "Invalid opcode: " ++ show opcode
        where getBinOp o = fromList [(8, BOAdd), (9, BOFAdd), (10, BOSub), (11, BOFSub),
                                     (12, BOMul), (13, BOFMul), (14, BOUDiv), (15, BOSDiv),
                                     (16, BOFDiv), (17, BOURem), (18, BOSRem), (19, BOFRem),
                                     (20, BOShL), (21, BOLShR), (22, BOAShR), (23, BOAnd),
                                     (24, BOOr), (25, BOXor)] ! o
              getConvOp o = fromList [(30, IDTrunc), (31, IDZExt), (32, IDSExt), (33, IDFPtoUI),
                                      (34, IDFPtoSI), (35, IDUItoFP), (36, IDSItoFP), (37, IDFPTrunc),
                                      (38, IDFPExt), (39, IDPtrToInt), (40, IDIntToPtr), (41, IDBitcast)] ! o
              toPairs xs = zip (stride 2 xs) (stride 2 (drop 1 xs))
              stride _ [] = []
              stride n (x:xs) = x : stride n (drop (n-1) xs)
              getPtrType (TDPtr t) = t
              getPtrType _ = TDVoid
              getImmInt (AI i) = i
              getImmInt _ = 0

-- TODO: fix for non-int constants
getArgDesc :: (String, FFI.ValueRef) -> IO ArgDesc
getArgDesc (vname, v) = do
    isC <- U.isConstant v
    t <- FFI.typeOf v >>= typeDesc2
    if isC
      then case t of
             TDInt _ _ -> do
                          cV <- FFI.constIntGetSExtValue v
                          return $ AI $ fromIntegral cV
             _ -> return AE
      else case t of
             TDLabel -> return $ AL vname
             _ -> return $ AV vname

--------------------------------------

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
        withCurrentBuilder_ $ \ bldPtr -> FFI.buildRet bldPtr a
        return terminate

instance Ret () () where
    ret' _ = do
        withCurrentBuilder_ $ FFI.buildRetVoid
        return terminate

withCurrentBuilder_ :: (FFI.BuilderRef -> IO a) -> CodeGenFunction r ()
withCurrentBuilder_ p = withCurrentBuilder p >> return ()

--------------------------------------

-- | Branch to the first basic block if the boolean is true, otherwise to the second basic block.
condBr :: Value Bool -- ^ Boolean to branch upon.
       -> BasicBlock -- ^ Target for true.
       -> BasicBlock -- ^ Target for false.
       -> CodeGenFunction r Terminate
condBr (Value b) (BasicBlock t1) (BasicBlock t2) = do
    withCurrentBuilder_ $ \ bldPtr -> FFI.buildCondBr bldPtr b t1 t2
    return terminate

--------------------------------------

-- | Unconditionally branch to the given basic block.
br :: BasicBlock  -- ^ Branch target.
   -> CodeGenFunction r Terminate
br (BasicBlock t) = do
    withCurrentBuilder_ $ \ bldPtr -> FFI.buildBr bldPtr t
    return terminate

--------------------------------------

-- | Branch table instruction.
switch :: (IsInteger a)
       => Value a                        -- ^ Value to branch upon.
       -> BasicBlock                     -- ^ Default branch target.
       -> [(ConstValue a, BasicBlock)]   -- ^ Labels and corresponding branch targets.
       -> CodeGenFunction r Terminate
switch (Value val) (BasicBlock dflt) arms = do
    withCurrentBuilder_ $ \ bldPtr -> do
        inst <- FFI.buildSwitch bldPtr val dflt (fromIntegral $ length arms)
        sequence_ [ FFI.addCase inst c b | (ConstValue c, BasicBlock b) <- arms ]
    return terminate

--------------------------------------

-- |Unwind the call stack until a function call performed with 'invoke' is reached.
-- I.e., throw a non-local exception.
unwind :: CodeGenFunction r Terminate
unwind = do
    withCurrentBuilder_ FFI.buildUnwind
    return terminate

-- |Inform the code generator that this code can never be reached.
unreachable :: CodeGenFunction r Terminate
unreachable = do
    withCurrentBuilder_ FFI.buildUnreachable
    return terminate

--------------------------------------

type FFIBinOp = FFI.BuilderRef -> FFI.ValueRef -> FFI.ValueRef -> U.CString -> IO FFI.ValueRef
type FFIConstBinOp = FFI.ValueRef -> FFI.ValueRef -> FFI.ValueRef


withArithmeticType ::
    (IsArithmetic c) =>
    (ArithmeticType c -> a -> CodeGenFunction r (v c)) ->
    (a -> CodeGenFunction r (v c))
withArithmeticType f = f arithmeticType

-- |Acceptable arguments to arithmetic binary instructions.
class ABinOp a b c | a b -> c where
    abinop :: FFIConstBinOp -> FFIBinOp -> a -> b -> CodeGenFunction r c

add :: (IsArithmetic c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
add =
    curry $ withArithmeticType $ \typ -> uncurry $ case typ of
      IntegerType  -> abinop FFI.constAdd  FFI.buildAdd
      FloatingType -> abinop FFI.constFAdd FFI.buildFAdd

sub :: (IsArithmetic c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
sub =
    curry $ withArithmeticType $ \typ -> uncurry $ case typ of
      IntegerType  -> abinop FFI.constSub  FFI.buildSub
      FloatingType -> abinop FFI.constFSub FFI.buildFSub

mul :: (IsArithmetic c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
mul =
    curry $ withArithmeticType $ \typ -> uncurry $ case typ of
      IntegerType  -> abinop FFI.constMul  FFI.buildMul
      FloatingType -> abinop FFI.constFMul FFI.buildFMul

iadd :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
iadd = abinop FFI.constAdd FFI.buildAdd
isub :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
isub = abinop FFI.constSub FFI.buildSub
imul :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
imul = abinop FFI.constMul FFI.buildMul

-- | signed or unsigned integer division depending on the type
idiv ::
   forall a b c r v. (IsInteger c, ABinOp a b (v c)) =>
   a -> b -> CodeGenFunction r (v c)
idiv =
   if isSigned (undefined :: c)
     then abinop FFI.constSDiv FFI.buildSDiv
     else abinop FFI.constUDiv FFI.buildUDiv
-- | signed or unsigned remainder depending on the type
irem ::
   forall a b c r v. (IsInteger c, ABinOp a b (v c)) =>
   a -> b -> CodeGenFunction r (v c)
irem =
   if isSigned (undefined :: c)
     then abinop FFI.constSRem FFI.buildSRem
     else abinop FFI.constURem FFI.buildURem

{-# DEPRECATED udiv "use idiv instead" #-}
{-# DEPRECATED sdiv "use idiv instead" #-}
{-# DEPRECATED urem "use irem instead" #-}
{-# DEPRECATED srem "use irem instead" #-}
udiv :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
udiv = abinop FFI.constUDiv FFI.buildUDiv
sdiv :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
sdiv = abinop FFI.constSDiv FFI.buildSDiv
urem :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
urem = abinop FFI.constURem FFI.buildURem
srem :: (IsInteger c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
srem = abinop FFI.constSRem FFI.buildSRem

fadd :: (IsFloating c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
fadd = abinop FFI.constFAdd FFI.buildFAdd
fsub :: (IsFloating c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
fsub = abinop FFI.constFSub FFI.buildFSub
fmul :: (IsFloating c, ABinOp a b (v c)) => a -> b -> CodeGenFunction r (v c)
fmul = abinop FFI.constFMul FFI.buildFMul

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

neg :: forall r a. (IsArithmetic a) => Value a -> CodeGenFunction r (Value a)
neg =
    withArithmeticType $ \typ -> case typ of
      IntegerType  -> \(Value x) -> buildUnOp FFI.buildNeg x
      FloatingType -> abinop FFI.constFSub FFI.buildFSub (value zero :: Value a)

ineg :: (IsInteger a) => Value a -> CodeGenFunction r (Value a)
ineg (Value x) = buildUnOp FFI.buildNeg x

fneg :: forall r a. (IsFloating a) => Value a -> CodeGenFunction r (Value a)
fneg = fsub (value zero :: Value a)
{-
fneg (Value x) = buildUnOp FFI.buildFNeg x
-}

inv :: (IsInteger a) => Value a -> CodeGenFunction r (Value a)
inv (Value x) = buildUnOp FFI.buildNot x

--------------------------------------

-- | Get a value from a vector.
extractelement :: (Pos n)
               => Value (Vector n a)               -- ^ Vector
               -> Value Word32                     -- ^ Index into the vector
               -> CodeGenFunction r (Value a)
extractelement (Value vec) (Value i) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildExtractElement bldPtr vec i

-- | Insert a value into a vector, nondestructive.
insertelement :: (Pos n)
              => Value (Vector n a)                -- ^ Vector
              -> Value a                           -- ^ Value to insert
              -> Value Word32                      -- ^ Index into the vector
              -> CodeGenFunction r (Value (Vector n a))
insertelement (Value vec) (Value e) (Value i) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildInsertElement bldPtr vec e i

-- | Permute vector.
shufflevector :: (Pos n, Pos m)
              => Value (Vector n a)
              -> Value (Vector n a)
              -> ConstValue (Vector m Word32)
              -> CodeGenFunction r (Value (Vector m a))
shufflevector (Value a) (Value b) (ConstValue mask) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ FFI.buildShuffleVector bldPtr a b mask


-- |Acceptable arguments to 'extractvalue' and 'insertvalue'.
class GetValue agg ix el | agg ix -> el where
    getIx :: agg -> ix -> CUInt

instance (GetField as i a, Nat i) => GetValue (Struct as) i a where
    getIx _ n = toNum n

instance (IsFirstClass a, Nat n) => GetValue (Array n a) Word32 a where
    getIx _ n = fromIntegral n

instance (IsFirstClass a, Nat n) => GetValue (Array n a) Word64 a where
    getIx _ n = fromIntegral n


instance (IsFirstClass a, Nat n, Nat (i1:*i0), (i1:*i0) :<: n) => GetValue (Array n a) (i1:*i0) a where
    getIx _ n = toNum n

instance (IsFirstClass a, Nat n, D0 :<: n) => GetValue (Array n a) D0 a where
    getIx _ n = toNum n

instance (IsFirstClass a, Nat n, D1 :<: n) => GetValue (Array n a) D1 a where
    getIx _ n = toNum n

instance (IsFirstClass a, Nat n, D2 :<: n) => GetValue (Array n a) D2 a where
    getIx _ n = toNum n

instance (IsFirstClass a, Nat n, D3 :<: n) => GetValue (Array n a) D3 a where
    getIx _ n = toNum n

instance (IsFirstClass a, Nat n, D4 :<: n) => GetValue (Array n a) D4 a where
    getIx _ n = toNum n

instance (IsFirstClass a, Nat n, D5 :<: n) => GetValue (Array n a) D5 a where
    getIx _ n = toNum n

instance (IsFirstClass a, Nat n, D6 :<: n) => GetValue (Array n a) D6 a where
    getIx _ n = toNum n

instance (IsFirstClass a, Nat n, D7 :<: n) => GetValue (Array n a) D7 a where
    getIx _ n = toNum n

instance (IsFirstClass a, Nat n, D8 :<: n) => GetValue (Array n a) D8 a where
    getIx _ n = toNum n

instance (IsFirstClass a, Nat n, D9 :<: n) => GetValue (Array n a) D9 a where
    getIx _ n = toNum n


-- | Get a value from an aggregate.
extractvalue :: forall r agg i a.
                GetValue agg i a
             => Value agg                   -- ^ Aggregate
             -> i                           -- ^ Index into the aggregate
             -> CodeGenFunction r (Value a)
extractvalue (Value agg) i =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $
        FFI.buildExtractValue bldPtr agg (getIx (undefined::agg) i)

-- | Insert a value into an aggregate, nondestructive.
insertvalue :: forall r agg i a.
               GetValue agg i a
            => Value agg                   -- ^ Aggregate
            -> Value a                     -- ^ Value to insert
            -> i                           -- ^ Index into the aggregate
            -> CodeGenFunction r (Value agg)
insertvalue (Value agg) (Value e) i =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $
        FFI.buildInsertValue bldPtr agg e (getIx (undefined::agg) i)


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

{-# DEPRECATED fptoui "use fptoint since it is type-safe with respect to signs" #-}
-- | Convert a floating point value to an unsigned integer.
fptoui :: (IsFloating a, IsInteger b, NumberOfElements n a, NumberOfElements n b) => Value a -> CodeGenFunction r (Value b)
fptoui = convert FFI.buildFPToUI

{-# DEPRECATED fptosi "use fptoint since it is type-safe with respect to signs" #-}
-- | Convert a floating point value to a signed integer.
fptosi :: (IsFloating a, IsInteger b, NumberOfElements n a, NumberOfElements n b) => Value a -> CodeGenFunction r (Value b)
fptosi = convert FFI.buildFPToSI

-- | Convert a floating point value to an integer.
-- It is mapped to @fptosi@ or @fptoui@ depending on the type @a@.
fptoint :: forall r n a b. (IsFloating a, IsInteger b, NumberOfElements n a, NumberOfElements n b) => Value a -> CodeGenFunction r (Value b)
fptoint =
   if isSigned (undefined :: b)
     then convert FFI.buildFPToSI
     else convert FFI.buildFPToUI


{-# DEPRECATED uitofp "use inttofp since it is type-safe with respect to signs" #-}
-- | Convert an unsigned integer to a floating point value.
uitofp :: (IsInteger a, IsFloating b, NumberOfElements n a, NumberOfElements n b) => Value a -> CodeGenFunction r (Value b)
uitofp = convert FFI.buildUIToFP

{-# DEPRECATED sitofp "use inttofp since it is type-safe with respect to signs" #-}
-- | Convert a signed integer to a floating point value.
sitofp :: (IsInteger a, IsFloating b, NumberOfElements n a, NumberOfElements n b) => Value a -> CodeGenFunction r (Value b)
sitofp = convert FFI.buildSIToFP

-- | Convert an integer to a floating point value.
-- It is mapped to @sitofp@ or @uitofp@ depending on the type @a@.
inttofp :: forall r n a b. (IsInteger a, IsFloating b, NumberOfElements n a, NumberOfElements n b) => Value a -> CodeGenFunction r (Value b)
inttofp =
   if isSigned (undefined :: a)
     then convert FFI.buildSIToFP
     else convert FFI.buildUIToFP


-- | Convert a pointer to an integer.
ptrtoint :: (IsInteger b, IsPrimitive b) => Value (Ptr a) -> CodeGenFunction r (Value b)
ptrtoint = convert FFI.buildPtrToInt

-- | Convert an integer to a pointer.
inttoptr :: (IsInteger a, IsType b) => Value a -> CodeGenFunction r (Value (Ptr b))
inttoptr = convert FFI.buildIntToPtr

-- | Convert between to values of the same size by just copying the bit pattern.
bitcast :: (IsFirstClass a, IsFirstClass b, IsSized a sa, IsSized b sb, sa :==: sb)
        => Value a -> CodeGenFunction r (Value b)
bitcast = convert FFI.buildBitCast

-- | Same as bitcast but instead of the '(:==:)' type class it uses type unification.
-- This way, properties like reflexivity, symmetry and transitivity
-- are obvious to the Haskell compiler.
bitcastUnify :: (IsFirstClass a, IsFirstClass b, IsSized a s, IsSized b s)
        => Value a -> CodeGenFunction r (Value b)
bitcastUnify = convert FFI.buildBitCast

type FFIConvert = FFI.BuilderRef -> FFI.ValueRef -> FFI.TypeRef -> U.CString -> IO FFI.ValueRef

convert :: forall a b r . (IsType b) => FFIConvert -> Value a -> CodeGenFunction r (Value b)
convert conv (Value a) =
    liftM Value $
    withCurrentBuilder $ \ bldPtr ->
      U.withEmptyCString $ conv bldPtr a (typeRef (undefined :: b))

--------------------------------------

data CmpPredicate =
    CmpEQ                       -- ^ equal
  | CmpNE                       -- ^ not equal
  | CmpGT                       -- ^ greater than
  | CmpGE                       -- ^ greater or equal
  | CmpLT                       -- ^ less than
  | CmpLE                       -- ^ less or equal
    deriving (Eq, Ord, Enum, Show, Typeable)

uintFromCmpPredicate :: CmpPredicate -> IntPredicate
uintFromCmpPredicate p =
   case p of
      CmpEQ -> IntEQ
      CmpNE -> IntNE
      CmpGT -> IntUGT
      CmpGE -> IntUGE
      CmpLT -> IntULT
      CmpLE -> IntULE

sintFromCmpPredicate :: CmpPredicate -> IntPredicate
sintFromCmpPredicate p =
   case p of
      CmpEQ -> IntEQ
      CmpNE -> IntNE
      CmpGT -> IntSGT
      CmpGE -> IntSGE
      CmpLT -> IntSLT
      CmpLE -> IntSLE

fpFromCmpPredicate :: CmpPredicate -> FPPredicate
fpFromCmpPredicate p =
   case p of
      CmpEQ -> FPOEQ
      CmpNE -> FPONE
      CmpGT -> FPOGT
      CmpGE -> FPOGE
      CmpLT -> FPOLT
      CmpLE -> FPOLE


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
    deriving (Eq, Ord, Enum, Show, Typeable)

fromIntPredicate :: IntPredicate -> CInt
fromIntPredicate p = fromIntegral (fromEnum p + 32)

toIntPredicate :: Int -> IntPredicate
toIntPredicate p = toEnum $ fromIntegral p - 32

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
    deriving (Eq, Ord, Enum, Show, Typeable)

fromFPPredicate :: FPPredicate -> CInt
fromFPPredicate p = fromIntegral (fromEnum p)

toFPPredicate :: Int -> FPPredicate
toFPPredicate p = toEnum $ fromIntegral p

-- |Acceptable operands to comparison instructions.
class CmpOp a b c d | a b -> c where
    cmpop :: FFIBinOp -> a -> b -> CodeGenFunction r (Value d)

instance CmpOp (Value a) (Value a) a d where
    cmpop op (Value a1) (Value a2) = buildBinOp op a1 a2

instance (IsConst a) => CmpOp a (Value a) a d where
    cmpop op a1 a2 = cmpop op (valueOf a1) a2

instance (IsConst a) => CmpOp (Value a) a a d where
    cmpop op a1 a2 = cmpop op a1 (valueOf a2)

class CmpRet c d | c -> d where
    cmpBld :: c -> CmpPredicate -> FFIBinOp
instance CmpRet Float   Bool where cmpBld _ = fcmpBld
instance CmpRet Double  Bool where cmpBld _ = fcmpBld
instance CmpRet FP128   Bool where cmpBld _ = fcmpBld
instance CmpRet Bool    Bool where cmpBld _ = ucmpBld
instance CmpRet Word8   Bool where cmpBld _ = ucmpBld
instance CmpRet Word16  Bool where cmpBld _ = ucmpBld
instance CmpRet Word32  Bool where cmpBld _ = ucmpBld
instance CmpRet Word64  Bool where cmpBld _ = ucmpBld
instance CmpRet Int8    Bool where cmpBld _ = scmpBld
instance CmpRet Int16   Bool where cmpBld _ = scmpBld
instance CmpRet Int32   Bool where cmpBld _ = scmpBld
instance CmpRet Int64   Bool where cmpBld _ = scmpBld
instance CmpRet (Ptr a) Bool where cmpBld _ = ucmpBld
instance (CmpRet a b, IsPrimitive a, Pos n) =>
            CmpRet (Vector n a) (Vector n b)
                             where cmpBld _ = cmpBld (undefined :: a)


{- |
Compare values of ordered types
and choose predicates according to the compared types.
Floating point numbers are compared in \"ordered\" mode,
that is @NaN@ operands yields 'False' as result.
Pointers are compared unsigned.
These choices are consistent with comparison in plain Haskell.
-}
cmp :: forall a b c d r.
   (CmpOp a b c d, CmpRet c d) =>
   CmpPredicate -> a -> b -> CodeGenFunction r (Value d)
cmp p = cmpop (cmpBld (undefined :: c) p)

ucmpBld :: CmpPredicate -> FFIBinOp
ucmpBld p = flip FFI.buildICmp (fromIntPredicate (uintFromCmpPredicate p))

scmpBld :: CmpPredicate -> FFIBinOp
scmpBld p = flip FFI.buildICmp (fromIntPredicate (sintFromCmpPredicate p))

fcmpBld :: CmpPredicate -> FFIBinOp
fcmpBld p = flip FFI.buildFCmp (fromFPPredicate (fpFromCmpPredicate p))


_ucmp :: (IsInteger c, CmpOp a b c d, CmpRet c d) =>
        CmpPredicate -> a -> b -> CodeGenFunction r (Value d)
_ucmp p = cmpop (flip FFI.buildICmp (fromIntPredicate (uintFromCmpPredicate p)))

_scmp :: (IsInteger c, CmpOp a b c d, CmpRet c d) =>
        CmpPredicate -> a -> b -> CodeGenFunction r (Value d)
_scmp p = cmpop (flip FFI.buildICmp (fromIntPredicate (sintFromCmpPredicate p)))

pcmp :: (CmpOp a b (Ptr c) d, CmpRet (Ptr c) d) =>
        IntPredicate -> a -> b -> CodeGenFunction r (Value d)
pcmp p = cmpop (flip FFI.buildICmp (fromIntPredicate p))


{-# DEPRECATED icmp "use cmp or pcmp instead" #-}
-- | Compare integers.
icmp :: (IsIntegerOrPointer c, CmpOp a b c d, CmpRet c d) =>
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
class CallArgs f g r | g -> r f, f r -> g where
    doCall :: Caller -> [FFI.ValueRef] -> f -> g

instance (CallArgs b b' r) => CallArgs (a -> b) (Value a -> b') r where
    doCall mkCall args f (Value arg) = doCall mkCall (arg : args) (f (undefined :: a))

--instance (CallArgs b b') => CallArgs (a -> b) (ConstValue a -> b') where
--    doCall mkCall args f (ConstValue arg) = doCall mkCall (arg : args) (f (undefined :: a))

instance CallArgs (IO a) (CodeGenFunction r (Value a)) r where
    doCall = doCallDef

doCallDef :: Caller -> [FFI.ValueRef] -> b -> CodeGenFunction r (Value a)
doCallDef mkCall args _ =
    withCurrentBuilder $ \ bld ->
      liftM Value $ mkCall bld (reverse args)

-- | Call a function with the given arguments.  The 'call' instruction is variadic, i.e., the number of arguments
-- it takes depends on the type of /f/.
call :: (CallArgs f g r) => Function f -> g
call (Value f) = doCall (U.makeCall f) [] (undefined :: f)

-- | Call a function with exception handling.
invoke :: (CallArgs f g r)
       => BasicBlock         -- ^Normal return point.
       -> BasicBlock         -- ^Exception return point.
       -> Function f         -- ^Function to call.
       -> g
invoke (BasicBlock norm) (BasicBlock expt) (Value f) =
    doCall (U.makeInvoke norm expt f) [] (undefined :: f)

-- | Call a function with the given arguments.  The 'call' instruction
-- is variadic, i.e., the number of arguments it takes depends on the
-- type of /f/.
-- This also sets the calling convention of the call to the function.
-- As LLVM itself defines, if the calling conventions of the calling
-- /instruction/ and the function being /called/ are different, undefined
-- behavior results.
callWithConv :: (CallArgs f g r) => FFI.CallingConvention -> Function f -> g
callWithConv cc (Value f) = doCall (U.makeCallWithCc cc f) [] (undefined :: f)

-- | Call a function with exception handling.
-- This also sets the calling convention of the call to the function.
-- As LLVM itself defines, if the calling conventions of the calling
-- /instruction/ and the function being /called/ are different, undefined
-- behavior results.
invokeWithConv :: (CallArgs f g r)
               => FFI.CallingConvention -- ^Calling convention
               -> BasicBlock         -- ^Normal return point.
               -> BasicBlock         -- ^Exception return point.
               -> Function f         -- ^Function to call.
               -> g
invokeWithConv cc (BasicBlock norm) (BasicBlock expt) (Value f) =
    doCall (U.makeInvokeWithCc cc norm expt f) [] (undefined :: f)

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
    getAllocArg :: a -> Value Word32
instance AllocArg (Value Word32) where
    getAllocArg = id
instance AllocArg (ConstValue Word32) where
    getAllocArg = value
instance AllocArg Word32 where
    getAllocArg = valueOf

-- could be moved to Util.Memory
-- FFI.buildMalloc deprecated since LLVM-2.7
-- XXX What's the type returned by malloc
-- | Allocate heap memory.
malloc :: forall a r s . (IsSized a s) => CodeGenFunction r (Value (Ptr a))
malloc = arrayMalloc (1::Word32)

{-
I use a pointer type as size parameter of 'malloc'.
This way I hope that the parameter has always the correct size (32 or 64 bit).
A side effect is that we can convert the result of 'getelementptr' using 'bitcast',
that does not suffer from the slow assembly problem. (bug #8281)
-}
foreign import ccall "&aligned_malloc_sizeptr"
   alignedMalloc :: FunPtr (Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8))

foreign import ccall "&aligned_free"
   alignedFree :: FunPtr (Ptr Word8 -> IO ())


{-
There is a bug in LLVM-2.7 and LLVM-2.8
(http://llvm.org/bugs/show_bug.cgi?id=8281)
that causes huge assembly times for expressions like
ptrtoint(getelementptr(zero,..)).
If you break those expressions into two statements
at separate lines, everything is fine.
But the C interface is too clever,
and rewrites two separate statements into a functional expression on a single line.
Such code is generated whenever you call
buildMalloc, buildArrayMalloc, sizeOf (called by buildMalloc), or alignOf.
One possible way is to write a getelementptr expression
containing a nullptr in a way
that hides the constant nature of nullptr.

    ptr <- alloca
    store (value zero) ptr
    z <- load ptr
    size <- bitcastUnify =<<
       getElementPtr (z :: Value (Ptr a)) (getAllocArg s, ())

However, I found that bitcast on pointers causes no problems.
Thus I switched to using pointers for size quantities.
This still allows for optimizations involving pointers.
-}

-- XXX What's the type returned by arrayMalloc?
-- | Allocate heap (array) memory.
arrayMalloc :: forall a n r s . (IsSized a n, AllocArg s) =>
               s -> CodeGenFunction r (Value (Ptr a)) -- XXX
arrayMalloc s = do
    func <- staticFunction alignedMalloc
--    func <- externFunction "malloc"

    size <- sizeOfArray (undefined :: a) (getAllocArg s)
    alignment <- alignOf (undefined :: a)
    bitcastUnify =<<
       call
          (func :: Function (Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8)))
          size
          alignment

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
        FFI.buildArrayAlloca bldPtr (typeRef (undefined :: a)) (case getAllocArg s of Value v -> v)

-- FFI.buildFree deprecated since LLVM-2.7
-- XXX What's the type of free?
-- | Free heap memory.
free :: (IsType a) => Value (Ptr a) -> CodeGenFunction r ()
free ptr = do
    func <- staticFunction alignedFree
--    func <- externFunction "free"
    _ <- call (func :: Function (Ptr Word8 -> IO ())) =<< bitcastUnify ptr
    return ()


-- | If we want to export that, then we should have a Size type
-- This is the official implementation,
-- but it suffers from the ptrtoint(gep) bug.
_sizeOf :: forall a r s . (IsSized a s) => a -> CodeGenFunction r (Value Word64)
_sizeOf a =
    liftIO $ liftM Value $
    FFI.sizeOf (typeRef a)

_alignOf :: forall a r s . (IsSized a s) => a -> CodeGenFunction r (Value Word64)
_alignOf a =
    liftIO $ liftM Value $
    FFI.alignOf (typeRef a)


-- Here are reimplementation from Constants.cpp that avoid the ptrtoint(gep) bug #8281.
-- see ConstantExpr::getSizeOf
sizeOfArray :: forall a r s . (IsSized a s) => a -> Value Word32 -> CodeGenFunction r (Value (Ptr Word8))
sizeOfArray _ len =
    bitcastUnify =<<
       getElementPtr (value zero :: Value (Ptr a)) (len, ())

-- see ConstantExpr::getAlignOf
alignOf :: forall a r s . (IsSized a s) => a -> CodeGenFunction r (Value (Ptr Word8))
alignOf _ =
    bitcastUnify =<<
       getElementPtr0 (value zero :: Value (Ptr (Struct (Bool, (a, ()))))) (d1, ())


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
      -> CodeGenFunction r ()
store (Value v) (Value p) = do
    withCurrentBuilder_ $ \ bldPtr ->
      FFI.buildStore bldPtr v p
    return ()

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
instance (GetElementPtr o i n, IsIndexArg a, Nat k) => GetElementPtr (Array k o) (a, i) n where
    getIxList _ (v, i) = getArg v : getIxList (undefined :: o) i

-- Index in Vector
instance (GetElementPtr o i n, IsIndexArg a, Pos k) => GetElementPtr (Vector k o) (a, i) n where
    getIxList _ (v, i) = getArg v : getIxList (undefined :: o) i

-- Index in Struct and PackedStruct.
-- The index has to be a type level integer to statically determine the record field type
instance (GetElementPtr o i n, GetField fs a o, Nat a) => GetElementPtr (Struct fs) (a, i) n where
    getIxList _ (v, i) = unConst (constOf (toNum v :: Word32)) : getIxList (undefined :: o) i
instance (GetElementPtr o i n, GetField fs a o, Nat a) => GetElementPtr (PackedStruct fs) (a, i) n where
    getIxList _ (v, i) = unConst (constOf (toNum v :: Word32)) : getIxList (undefined :: o) i

class GetField as i a | as i -> a
instance GetField (a, as) D0 a
instance (GetField as i b, Succ i i') => GetField (a, as) i' b

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

-- | Like getElementPtr, but with an initial index that is 0.
-- This is useful since any pointer first need to be indexed off the pointer, and then into
-- its actual value.  This first indexing is often with 0.
getElementPtr0 :: (GetElementPtr o i n) =>
                  Value (Ptr o) -> i -> CodeGenFunction r (Value (Ptr n))
getElementPtr0 p i = getElementPtr p (0::Word32, i)

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
