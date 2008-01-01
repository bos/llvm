{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}

module LLVM.Core.Builder
    (
      Instruction(..)
    , BasicBlock(..)

    , buildCall
    , buildRet
    , buildGEP

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
    , gep

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

    {-
    -- * Miscellaneous instructions
    , phi
    , call
    , select
    , vaArg
    , extractElement
    , insertElement
    , shuffleVector
    -}
    ) where

import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Control.Monad (forM_)
import Data.Typeable (Typeable)
import Foreign.C.String (CString, withCString)
import Foreign.ForeignPtr (FinalizerPtr, ForeignPtr, newForeignPtr,
                           withForeignPtr)
import Foreign.Marshal.Array (withArrayLen)
import Prelude hiding (and, not, or)

import qualified LLVM.Core.FFI as FFI
import qualified LLVM.Core.Instruction as I
import qualified LLVM.Core.Type as T
import qualified LLVM.Core.Value as V


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

newtype Instruction a = Instruction V.AnyValue
    deriving (V.DynamicValue, Typeable, V.Value)


instruction :: IO FFI.ValueRef -> IO (Instruction t)
instruction = fmap (Instruction . V.mkAnyValue)

buildGEP :: (V.Value p, V.Value i) => Builder -> p -> [i] -> String
         -> IO (Instruction a)
buildGEP bld ptr indices name =
    withBuilder bld $ \bldPtr ->
      withCString name $ \namePtr ->
        withArrayLen (map V.valueRef indices) $ \idxLen idxPtr ->
          instruction $ FFI.buildGEP bldPtr (V.valueRef ptr) idxPtr
                                  (fromIntegral idxLen) namePtr

buildRet :: V.Value a => Builder -> a -> IO (Instruction a)
buildRet bld val =
    withBuilder bld $ \bldPtr ->
      instruction $ FFI.buildRet bldPtr (V.valueRef val)

buildCall :: Builder -> V.Function a -> [V.AnyValue] -> String
          -> IO (Instruction a)
buildCall bld func args name =
    withBuilder bld $ \bldPtr ->
      withArrayLen (map V.valueRef args) $ \argLen argPtr ->
        withCString name $ \namePtr ->
          instruction $ FFI.buildCall bldPtr (V.valueRef func) argPtr
                          (fromIntegral argLen) namePtr

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

add :: (T.Arithmetic t, V.Value v, V.TypedValue v t)
       => Builder -> String -> v -> v -> IO (Instruction t)
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

switch :: (T.Integer t, V.TypedValue v t)
          => Builder -> v -> BasicBlock -> [(v, BasicBlock)]
          -> IO (Instruction T.Void)
switch bld val noMatch cases =
    withBuilder bld $ \bldPtr -> do
        inst <- FFI.buildSwitch bldPtr (V.valueRef val)
                        (V.valueRef noMatch) (fromIntegral $ length cases)
        forM_ (map (V.valueRef *** V.valueRef) cases) $
            uncurry (FFI.addCase inst)
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

gep :: (T.Sequence s e, V.TypedValue p s,
        T.Integer t, V.TypedValue i t,
        T.Type u) =>
       Builder -> String -> p -> [i] -> IO (Instruction u)
gep bld name ptr idxs =
    withBuilder bld $ \bldPtr ->
        withCString name $ \namePtr ->
          withArrayLen (map V.valueRef idxs) $ \idxLen idxPtr ->
            instruction $ FFI.buildGEP bldPtr (V.valueRef ptr) idxPtr
                            (fromIntegral idxLen) namePtr
