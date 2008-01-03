{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , FunctionalDependencies
  , MultiParamTypeClasses
  , UndecidableInstances
  #-}

module LLVM.Core.Builder
    (
      Instruction(..)
    , BasicBlock(..)

    -- * Instruction building
    , createBuilder

    , positionBefore
    , positionAtEnd

    -- * Terminators
    , retVoid
    , ret
    , br
    , condBr
    , switch
    , invoke
    , unwind
    , unreachable

    -- * Arithmetic
    , add
    , sub
    , mul
    , uDiv
    , sDiv
    , fDiv
    , uRem
    , sRem
    , fRem
    , shl
    , lShr
    , aShr
    , and
    , or
    , xor
    , neg
    , not

    -- * Memory
    , malloc
    , arrayMalloc
    , alloca
    , arrayAlloca
    , free
    , load
    , store
    , getElementPtr

    -- * Casts
    , trunc
    , zExt
    , sExt
    , fpToUI
    , fpToSI
    , uiToFP
    , siToFP
    , fpTrunc
    , fpExt
    , ptrToInt
    , intToPtr
    , bitCast

    -- * Comparisons
    , icmp
    , fcmp

    -- * Miscellaneous instructions
    , call
    , call_
    , extractElement
    , insertElement
    , phi
    , select
    , vaArg
    , shuffleVector
    ) where

import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Control.Monad (forM_)
import Data.Typeable (Typeable)
import Foreign.C.String (CString, withCString)
import Foreign.ForeignPtr (FinalizerPtr, ForeignPtr, newForeignPtr,
                           withForeignPtr)
import Foreign.Marshal.Array (withArray, withArrayLen)
import Prelude hiding (and, not, or)

import qualified LLVM.Core.FFI as FFI
import qualified LLVM.Core.Instruction as I
import qualified LLVM.Core.Type as T
import qualified LLVM.Core.Value as V
import LLVM.Core.Type ((:->)(..))
import LLVM.Core.Value (Instruction(..))


newtype Builder = Builder {
      fromBuilder :: ForeignPtr FFI.Builder
    }
    deriving (Typeable)

newtype BasicBlock = BasicBlock V.AnyValue
    deriving (V.DynamicValue, Typeable, V.Value)

withBuilder :: Builder -> (FFI.BuilderRef -> IO a) -> IO a
withBuilder = withForeignPtr . fromBuilder

createBuilder :: IO Builder
createBuilder = do
  final <- h2c_builder FFI.disposeBuilder
  ptr <- FFI.createBuilder
  Builder <$> newForeignPtr final ptr

foreign import ccall "wrapper" h2c_builder
    :: (FFI.BuilderRef -> IO ()) -> IO (FinalizerPtr a)

positionBefore :: Builder -> Instruction a -> IO ()
positionBefore bld insn =
    withBuilder bld $ \bldPtr ->
      FFI.positionBefore bldPtr (V.valueRef insn)

positionAtEnd :: Builder -> BasicBlock -> IO ()
positionAtEnd bld bblk =
    withBuilder bld $ \bldPtr ->
      FFI.positionAtEnd bldPtr (V.valueRef bblk)

instruction :: IO FFI.ValueRef -> IO (Instruction t)
instruction = fmap (Instruction . V.mkAnyValue)

unary :: (V.Value a)
         => (FFI.BuilderRef -> FFI.ValueRef -> CString -> IO FFI.ValueRef)
      -> Builder -> String -> a -> IO (Instruction t)
unary ffi bld name a =
    withBuilder bld $ \bldPtr ->
      withCString name $ \namePtr ->
        Instruction . V.mkAnyValue <$>
        ffi bldPtr (V.valueRef a) namePtr

binary :: (V.Value a, V.Value b)
          => (FFI.BuilderRef -> FFI.ValueRef -> FFI.ValueRef -> CString
              -> IO FFI.ValueRef)
          -> Builder -> String -> a -> b -> IO (Instruction t)
binary ffi bld name a b =
    withBuilder bld $ \bldPtr ->
      withCString name $ instruction . ffi bldPtr (V.valueRef a) (V.valueRef b) 

add :: (T.Arithmetic t,
        V.Value a, V.TypedValue a t,
        V.Value b, V.TypedValue b t)
       => Builder -> String -> a -> b -> IO (Instruction t)
add = binary FFI.buildAdd

sub :: (T.Arithmetic t, V.Value v, V.TypedValue v t)
       => Builder -> String -> v -> v -> IO (Instruction t)
sub = binary FFI.buildSub

mul :: (T.Arithmetic t, V.Value v, V.TypedValue v t)
       => Builder -> String -> v -> v -> IO (Instruction t)
mul = binary FFI.buildSub

uDiv :: (T.Integer t, V.Value v, V.TypedValue v t)
       => Builder -> String -> v -> v -> IO (Instruction t)
uDiv = binary FFI.buildUDiv

sDiv :: (T.Integer t, V.Value v, V.TypedValue v t)
       => Builder -> String -> v -> v -> IO (Instruction t)
sDiv = binary FFI.buildSDiv

fDiv :: (T.Real t, V.Value v, V.TypedValue v t)
       => Builder -> String -> v -> v -> IO (Instruction t)
fDiv = binary FFI.buildFDiv

uRem :: (T.Integer t, V.Value v, V.TypedValue v t)
       => Builder -> String -> v -> v -> IO (Instruction t)
uRem = binary FFI.buildURem

sRem :: (T.Integer t, V.Value v, V.TypedValue v t)
       => Builder -> String -> v -> v -> IO (Instruction t)
sRem = binary FFI.buildSRem

fRem :: (T.Real t, V.Value v, V.TypedValue v t)
       => Builder -> String -> v -> v -> IO (Instruction t)
fRem = binary FFI.buildFRem

shl :: (T.Integer t, V.Value v, V.TypedValue v t)
       => Builder -> String -> v -> v -> IO (Instruction t)
shl = binary FFI.buildShl

lShr :: (T.Integer t, V.Value v, V.TypedValue v t)
        => Builder -> String -> v -> v -> IO (Instruction t)
lShr = binary FFI.buildLShr

aShr :: (T.Integer t, V.TypedValue v t)
        => Builder -> String -> v -> v -> IO (Instruction t)
aShr = binary FFI.buildAShr

and :: (T.Integer t, V.TypedValue v t)
       => Builder -> String -> v -> v -> IO (Instruction t)
and = binary FFI.buildAnd

or :: (T.Integer t, V.TypedValue v t)
      => Builder -> String -> v -> v -> IO (Instruction t)
or = binary FFI.buildOr

xor :: (T.Integer t, V.TypedValue v t)
       => Builder -> String -> v -> v -> IO (Instruction t)
xor = binary FFI.buildAnd

neg :: (T.Arithmetic t, V.TypedValue v t)
       => Builder -> String -> v -> IO (Instruction t)
neg = unary FFI.buildNeg

not :: (T.Arithmetic t, V.TypedValue v t)
       => Builder -> String -> v -> IO (Instruction t)
not = unary FFI.buildNot

typed :: (V.Value v, T.Type s, T.Type t)
         => (FFI.BuilderRef -> FFI.ValueRef -> FFI.TypeRef -> CString
             -> IO FFI.ValueRef)
         -> Builder -> String -> v -> s -> IO (Instruction t)
typed ffi bld name a t =
    withBuilder bld $ \bldPtr ->
      withCString name $ \namePtr ->
        Instruction . V.mkAnyValue <$> ffi bldPtr (V.valueRef a) (T.typeRef t) namePtr

trunc :: (T.Integer s, V.TypedValue v s, T.Integer t)
         => Builder -> String -> v -> s -> IO (Instruction t)
trunc = typed FFI.buildTrunc

zExt :: (T.Integer s, V.TypedValue v s, T.Integer t)
         => Builder -> String -> v -> s -> IO (Instruction t)
zExt = typed FFI.buildZExt

sExt :: (T.Integer s, V.TypedValue v s, T.Integer t)
         => Builder -> String -> v -> s -> IO (Instruction t)
sExt = typed FFI.buildSExt

fpToUI :: (T.Integer s, V.TypedValue v s, T.Real t)
         => Builder -> String -> v -> s -> IO (Instruction t)
fpToUI = typed FFI.buildFPToUI

fpToSI :: (T.Integer s, V.TypedValue v s, T.Real t)
         => Builder -> String -> v -> s -> IO (Instruction t)
fpToSI = typed FFI.buildFPToSI

uiToFP :: (T.Real s, V.TypedValue v s, T.Integer t)
         => Builder -> String -> v -> s -> IO (Instruction t)
uiToFP = typed FFI.buildUIToFP

siToFP :: (T.Real s, V.TypedValue v s, T.Integer t)
         => Builder -> String -> v -> s -> IO (Instruction t)
siToFP = typed FFI.buildSIToFP

fpTrunc :: (T.Real s, V.TypedValue v s, T.Real t)
         => Builder -> String -> v -> s -> IO (Instruction t)
fpTrunc = typed FFI.buildFPTrunc

fpExt :: (T.Real s, V.TypedValue v s, T.Real t)
         => Builder -> String -> v -> s -> IO (Instruction t)
fpExt = typed FFI.buildFPExt

ptrToInt :: (V.TypedValue (T.Pointer s) s, T.Integer t)
            => Builder -> String -> T.Pointer s -> s -> IO (Instruction t)
ptrToInt = typed FFI.buildPtrToInt

intToPtr :: (T.Integer s, V.TypedValue v s, T.Type t)
            => Builder -> String -> v -> t -> IO (Instruction (T.Pointer t))
intToPtr = typed FFI.buildIntToPtr

bitCast :: (V.TypedValue v s, T.Type t)
           => Builder -> String -> v -> s -> IO (Instruction t)
bitCast = typed FFI.buildBitCast

fcmp :: (T.Real t, V.TypedValue v t)
        => Builder -> String -> I.RealPredicate -> v -> v
        -> IO (Instruction T.Int1)
fcmp bld name p = binary (flip FFI.buildFCmp (I.fromRP p)) bld name

icmp :: (T.Integer t, V.TypedValue v t)
        => Builder -> String -> I.IntPredicate -> v -> v
        -> IO (Instruction T.Int1)
icmp bld name p = binary (flip FFI.buildICmp (I.fromIP p)) bld name

retVoid :: Builder -> IO (Instruction T.Void)
retVoid bld = withBuilder bld $ instruction . FFI.buildRetVoid

ret :: (T.FirstClass t, V.TypedValue v t) => Builder -> v -> IO (Instruction t)
ret bld v =
    withBuilder bld $ \bldPtr ->
      instruction $ FFI.buildRet bldPtr (V.valueRef v)

br :: Builder -> BasicBlock -> IO (Instruction T.Void)
br bld bblk =
    withBuilder bld $ \bldPtr ->
      instruction $ FFI.buildBr bldPtr (V.valueRef bblk)

condBr :: (V.TypedValue v T.Int1)
          => Builder -> v -> BasicBlock -> BasicBlock
          -> IO (Instruction T.Void)
condBr bld bit true false =
    withBuilder bld $ \bldPtr ->
      instruction $ FFI.buildCondBr bldPtr (V.valueRef bit)
                      (V.valueRef true) (V.valueRef false)

unwrap :: (V.Value a, V.Value b) => (a, b) -> (FFI.ValueRef, FFI.ValueRef)
unwrap = V.valueRef *** V.valueRef

switch :: (T.Integer t, V.TypedValue v t)
          => Builder -> v -> BasicBlock -> [(v, BasicBlock)]
          -> IO (Instruction T.Void)
switch bld val noMatch cases =
    withBuilder bld $ \bldPtr -> do
        inst <- FFI.buildSwitch bldPtr (V.valueRef val)
                        (V.valueRef noMatch) (fromIntegral $ length cases)
        forM_ (map unwrap cases) $ uncurry (FFI.addCase inst)
        instruction $ return inst

invoke :: Builder -> String -> V.Function t -> [V.AnyValue]
       -> BasicBlock -> BasicBlock -> IO (Instruction T.Void)
invoke bld name func args thenBlk catchBlk =
  withBuilder bld $ \bldPtr ->
    withCString name $ \namePtr ->
      withArrayLen (map V.valueRef args) $ \argLen argPtr ->
        instruction $ FFI.buildInvoke bldPtr (V.valueRef func) argPtr
                        (fromIntegral argLen) (V.valueRef thenBlk)
                        (V.valueRef catchBlk) namePtr

unwind :: Builder -> IO (Instruction T.Void)
unwind bld = withBuilder bld $ instruction . FFI.buildUnwind

unreachable :: Builder -> IO (Instruction T.Void)
unreachable bld = withBuilder bld $ instruction . FFI.buildUnreachable

allocWith :: (T.Type t)
             => (FFI.BuilderRef -> FFI.TypeRef -> CString -> IO FFI.ValueRef)
             -> Builder -> String -> t -> IO FFI.ValueRef
allocWith ffi bld name typ =
    withBuilder bld $ \bldPtr ->
      withCString name $ ffi bldPtr (T.typeRef typ)

arrayAllocWith :: (T.Type t, T.Integer n, V.TypedValue v n)
               => (FFI.BuilderRef -> FFI.TypeRef -> FFI.ValueRef -> CString
                   -> IO FFI.ValueRef)
               -> Builder -> String -> t -> v -> IO FFI.ValueRef
arrayAllocWith ffi bld name typ count =
    withBuilder bld $ \bldPtr ->
      withCString name $ ffi bldPtr (T.typeRef typ) (V.valueRef count)

malloc :: (T.Type t) => Builder -> String -> t -> IO (Instruction (T.Array t))
malloc bld name typ = instruction $ allocWith FFI.buildMalloc bld name typ

arrayMalloc :: (T.Type t, V.TypedValue v T.Int32)
               => Builder -> String -> t -> v -> IO (Instruction (T.Array t))
arrayMalloc bld name typ count =
    instruction $ arrayAllocWith FFI.buildArrayMalloc bld name typ count

alloca :: (T.Type t)
          => Builder -> String -> t -> IO (Instruction (T.Pointer t))
alloca bld name typ = instruction $ allocWith FFI.buildAlloca bld name typ

arrayAlloca :: (T.Type t, V.TypedValue v T.Int32)
               => Builder -> String -> t -> v -> IO (Instruction (T.Pointer t))
arrayAlloca bld name typ count =
    instruction $ arrayAllocWith FFI.buildArrayAlloca bld name typ count

free :: (V.TypedValue v (T.Pointer t))
        => Builder -> v -> IO (Instruction T.Void)
free bld ary =
    withBuilder bld $ \bldPtr ->
      instruction $ FFI.buildFree bldPtr (V.valueRef ary)

load :: (V.TypedValue v (T.Pointer t))
        => Builder -> String -> v -> IO (Instruction t)
load bld name ptr =
    withBuilder bld $ \bldPtr ->
        instruction $ withCString name $ FFI.buildLoad bldPtr (V.valueRef ptr)

store :: (V.TypedValue v t, V.TypedValue p (T.Pointer t))
        => Builder -> v -> p -> IO (Instruction T.Void)
store bld val ptr =
    withBuilder bld $ \bldPtr ->
      instruction $ FFI.buildStore bldPtr (V.valueRef val) (V.valueRef ptr) 

getElementPtr :: (T.Sequence s e, V.TypedValue p s,
                  T.Integer t, V.TypedValue i t)
                 => Builder -> String -> p -> [i]
                 -> IO (Instruction (T.Pointer e))
getElementPtr bld name ptr idxs =
    withBuilder bld $ \bldPtr ->
        withCString name $ \namePtr ->
          withArrayLen (map V.valueRef idxs) $ \idxLen idxPtr ->
            instruction $ FFI.buildGEP bldPtr (V.valueRef ptr) idxPtr
                            (fromIntegral idxLen) namePtr

class Params t v | t -> v where
    toAnyList :: t -> v -> [V.AnyValue]

instance (V.TypedValue c a, Params b d) => Params (a :-> b) (c :-> d) where
    toAnyList t (a :-> b) = V.anyValue a : toAnyList (T.cdr t) b

instance V.TypedValue v T.Int32 => Params T.Int32 v where
    toAnyList _ a = [V.anyValue a]

callRef :: (T.Params p, Params p v)
           => Builder -> String -> V.Function p -> v -> IO FFI.ValueRef
callRef bld name func args = do
    let argList = init $ toAnyList (T.params (V.typeOf func)) args
    withBuilder bld $ \bldPtr ->
      withArrayLen (map V.valueRef argList) $ \argLen argPtr ->
        withCString name $ \namePtr ->
          FFI.buildCall bldPtr (V.valueRef func) argPtr
                 (fromIntegral argLen) namePtr

call :: (T.Params p, Params p v, T.FirstClass t)
        => Builder -> String -> V.Function p -> v
     -> IO (Instruction t)
call bld name func args = instruction $ callRef bld name func args

call_ :: (T.Params p, Params p v)
         => Builder -> String -> V.Function p -> v -> IO ()
call_ bld name func args = callRef bld name func args >> return ()

extractElement :: (V.TypedValue v (T.Vector t),
                   V.TypedValue i T.Int32)
                  => Builder -> String -> v -> i -> IO (Instruction t)
extractElement bld name vec idx =
    withBuilder bld $ \bldPtr ->
        withCString name $ \namePtr ->
            instruction $ FFI.buildExtractElement bldPtr (V.valueRef vec)
                            (V.valueRef idx) namePtr

insertElement :: (V.TypedValue v (T.Vector t),
                  V.TypedValue e t,
                  V.TypedValue i T.Int32)
                  => Builder -> String -> v -> e -> i -> IO (Instruction t)
insertElement bld name vec elt idx =
    withBuilder bld $ \bldPtr ->
        withCString name $ \namePtr ->
            instruction $ FFI.buildInsertElement bldPtr (V.valueRef vec)
                            (V.valueRef elt) (V.valueRef idx) namePtr

phi :: (V.TypedValue v t)
       => Builder -> String -> t -> [(v, BasicBlock)] -> IO (Instruction t)
phi bld name typ incoming =
    withBuilder bld $ \bldPtr ->
      withCString name $ \namePtr -> do
        inst <- FFI.buildPhi bldPtr (T.typeRef typ) namePtr
        let (vals, bblks) = unzip . map unwrap $ incoming
        withArrayLen vals $ \count valPtr ->
          withArray bblks $ \bblkPtr ->
            FFI.addIncoming inst valPtr bblkPtr (fromIntegral count)
        instruction $ return inst

select :: (V.TypedValue p T.Int1,
           V.TypedValue v t)
          => Builder -> String -> p -> v -> v -> IO (Instruction t)
select bld name bit true false =
    withBuilder bld $ \bldPtr ->
      withCString name $ \namePtr -> do
        instruction $ FFI.buildSelect bldPtr (V.valueRef bit)
                        (V.valueRef true) (V.valueRef false) namePtr

vaArg :: (V.Value v, T.Type t)
         => Builder -> String -> v -> t -> IO (Instruction t)
vaArg bld name valist typ =
    withBuilder bld $ \bldPtr ->
      withCString name $ \namePtr ->
        instruction $ FFI.buildVAArg bldPtr (V.valueRef valist)
                        (T.typeRef typ) namePtr

shuffleVector :: (V.TypedValue v (T.Vector t),
                  V.TypedValue m (T.Vector T.Int32))
                 => Builder -> String -> v -> v -> m
                 -> IO (Instruction (T.Vector t))
shuffleVector bld name a b mask =
    withBuilder bld $ \bldPtr ->
      withCString name $ \namePtr ->
        instruction $ FFI.buildShuffleVector bldPtr (V.valueRef a)
                        (V.valueRef b) (V.valueRef mask) namePtr
