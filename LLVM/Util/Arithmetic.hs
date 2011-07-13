{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP, FlexibleInstances, ScopedTypeVariables, FlexibleContexts, UndecidableInstances, TypeSynonymInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module LLVM.Util.Arithmetic(
    TValue,
    Cmp(..),
    (%==), (%/=), (%<), (%<=), (%>), (%>=),
    (%&&), (%||),
    (?), (??),
    retrn, set,
    ArithFunction, arithFunction,
    UnwrapArgs, toArithFunction,
    recursiveFunction,
    CallIntrinsic,
    ) where
import Data.Word
import Data.Int
import qualified Data.TypeLevel.Num as TypeNum
import qualified LLVM.Core as LLVM
import LLVM.Core hiding (cmp, )
import LLVM.Util.Loop(mapVector, mapVector2)

-- |Synonym for @CodeGenFunction r (Value a)@.
type TValue r a = CodeGenFunction r (Value a)

{-# DEPRECATED cmp "use LLVM.Core.cmp instead" #-}
class (CmpRet a b) => Cmp a b | a -> b where
    cmp :: IntPredicate -> Value a -> Value a -> TValue r b

instance Cmp Bool Bool where cmp = icmp
instance Cmp Word8 Bool where cmp = icmp
instance Cmp Word16 Bool where cmp = icmp
instance Cmp Word32 Bool where cmp = icmp
instance Cmp Word64 Bool where cmp = icmp
instance Cmp Int8 Bool where cmp = icmp . adjSigned
instance Cmp Int16 Bool where cmp = icmp . adjSigned
instance Cmp Int32 Bool where cmp = icmp . adjSigned
instance Cmp Int64 Bool where cmp = icmp . adjSigned
instance Cmp Float Bool where cmp = fcmp . adjFloat
instance Cmp Double Bool where cmp = fcmp . adjFloat
instance Cmp FP128 Bool where cmp = fcmp . adjFloat
{-
instance (Pos n) => Cmp (Vector n Bool) (Vector n Bool) where cmp = icmp
instance (Pos n) => Cmp (Vector n Word8) (Vector n Bool) where cmp = icmp
instance (Pos n) => Cmp (Vector n Word16) (Vector n Bool) where cmp = icmp
instance (Pos n) => Cmp (Vector n Word32) (Vector n Bool) where cmp = icmp
instance (Pos n) => Cmp (Vector n Word64) (Vector n Bool) where cmp = icmp
instance (Pos n) => Cmp (Vector n Int8) (Vector n Bool) where cmp = icmp . adjSigned
instance (Pos n) => Cmp (Vector n Int16) (Vector n Bool) where cmp = icmp . adjSigned
instance (Pos n) => Cmp (Vector n Int32) (Vector n Bool) where cmp = icmp . adjSigned
instance (Pos n) => Cmp (Vector n Int64) (Vector n Bool) where cmp = icmp . adjSigned
instance (Pos n) => Cmp (Vector n Float) (Vector n Bool) where cmp = fcmp . adjFloat
instance (Pos n) => Cmp (Vector n Double) (Vector n Bool) where cmp = fcmp . adjFloat
instance (Pos n) => Cmp (Vector n FP128) (Vector n Bool) where cmp = fcmp . adjFloat
-}
instance (Pos n) => Cmp (Vector n Float) (Vector n Bool) where
    cmp op = mapVector2 (fcmp (adjFloat op))
instance (Pos n) => Cmp (Vector n Word32) (Vector n Bool) where
    cmp op = mapVector2 (cmp op)

adjSigned :: IntPredicate -> IntPredicate
adjSigned IntUGT = IntSGT
adjSigned IntUGE = IntSGE
adjSigned IntULT = IntSLT
adjSigned IntULE = IntSLE
adjSigned p = p

adjFloat :: IntPredicate -> FPPredicate
adjFloat IntEQ  = FPOEQ
adjFloat IntNE  = FPONE
adjFloat IntUGT = FPOGT
adjFloat IntUGE = FPOGE
adjFloat IntULT = FPOLT
adjFloat IntULE = FPOLE
adjFloat _ = error "adjFloat"

infix  4  %==, %/=, %<, %<=, %>=, %>
-- |Comparison functions.
(%==), (%/=), (%<), (%<=), (%>), (%>=) :: (CmpRet a b) => TValue r a -> TValue r a -> TValue r b
(%==) = binop $ LLVM.cmp CmpEQ
(%/=) = binop $ LLVM.cmp CmpNE
(%>)  = binop $ LLVM.cmp CmpGT
(%>=) = binop $ LLVM.cmp CmpGE
(%<)  = binop $ LLVM.cmp CmpLT
(%<=) = binop $ LLVM.cmp CmpLE

infixr 3  %&&
infixr 2  %||
-- |Lazy and.
(%&&) :: TValue r Bool -> TValue r Bool -> TValue r Bool
a %&& b = a ? (b, return (valueOf False))
-- |Lazy or.
(%||) :: TValue r Bool -> TValue r Bool -> TValue r Bool
a %|| b = a ? (return (valueOf True), b)

infix  0 ?
-- |Conditional, returns first element of the pair when condition is true, otherwise second.
(?) :: (IsFirstClass a) => TValue r Bool -> (TValue r a, TValue r a) -> TValue r a
c ? (t, f) = do
    lt <- newBasicBlock
    lf <- newBasicBlock
    lj <- newBasicBlock
    c' <- c
    condBr c' lt lf
    defineBasicBlock lt
    rt <- t
    lt' <- getCurrentBasicBlock
    br lj
    defineBasicBlock lf
    rf <- f
    lf' <- getCurrentBasicBlock
    br lj
    defineBasicBlock lj
    phi [(rt, lt'), (rf, lf')]

infix 0 ??
(??) :: (IsFirstClass a, CmpRet a b) => TValue r b -> (TValue r a, TValue r a) -> TValue r a
c ?? (t, f) = do
    c' <- c
    t' <- t
    f' <- f
    select c' t' f'

-- | Return a value from an 'arithFunction'.
retrn :: (Ret (Value a) r) => TValue r a -> CodeGenFunction r ()
retrn x = x >>= ret

-- | Use @x <- set $ ...@ to make a binding.
set :: TValue r a -> (CodeGenFunction r (TValue r a))
set x = do x' <- x; return (return x')

instance (Show (TValue r a))
instance (Eq (TValue r a))
instance (Ord (TValue r a))

instance (IsArithmetic a, Cmp a b, Num a, IsConst a) => Num (TValue r a) where
    (+) = binop add
    (-) = binop sub
    (*) = binop mul
    negate = (>>= neg)
    abs x = x %< 0 ?? (-x, x)
    signum x = x %< 0 ?? (-1, x %> 0 ?? (1, 0))
    fromInteger = return . valueOf . fromInteger

instance (IsArithmetic a, Cmp a b, Num a, IsConst a) => Enum (TValue r a) where
    succ x = x + 1
    pred x = x - 1
    fromEnum _ = error "CodeGenFunction Value: fromEnum"
    toEnum = fromIntegral

instance (IsArithmetic a, Cmp a b, Num a, IsConst a) => Real (TValue r a) where
    toRational _ = error "CodeGenFunction Value: toRational"

instance (Cmp a b, Num a, IsConst a, IsInteger a) => Integral (TValue r a) where
    quot = binop idiv
    rem  = binop irem
    quotRem x y = (quot x y, rem x y)
    toInteger _ = error "CodeGenFunction Value: toInteger"

instance (Cmp a b, Fractional a, IsConst a, IsFloating a) => Fractional (TValue r a) where
    (/) = binop fdiv
    fromRational = return . valueOf . fromRational

instance (Cmp a b, Fractional a, IsConst a, IsFloating a) => RealFrac (TValue r a) where
    properFraction _ = error "CodeGenFunction Value: properFraction"

instance (Cmp a b, CallIntrinsic a, Floating a, IsConst a, IsFloating a) => Floating (TValue r a) where
    pi = return $ valueOf pi
    sqrt = callIntrinsic1 "sqrt"
    sin = callIntrinsic1 "sin"
    cos = callIntrinsic1 "cos"
    (**) = callIntrinsic2 "pow"
    exp = callIntrinsic1 "exp"
    log = callIntrinsic1 "log"

    asin _ = error "LLVM missing intrinsic: asin"
    acos _ = error "LLVM missing intrinsic: acos"
    atan _ = error "LLVM missing intrinsic: atan"

    sinh x           = (exp x - exp (-x)) / 2
    cosh x           = (exp x + exp (-x)) / 2
    asinh x          = log (x + sqrt (x*x + 1))
    acosh x          = log (x + sqrt (x*x - 1))
    atanh x          = (log (1 + x) - log (1 - x)) / 2

instance (Cmp a b, CallIntrinsic a, RealFloat a, IsConst a, IsFloating a) => RealFloat (TValue r a) where
    floatRadix _ = floatRadix (undefined :: a)
    floatDigits _ = floatDigits (undefined :: a)
    floatRange _ = floatRange (undefined :: a)
    decodeFloat _ = error "CodeGenFunction Value: decodeFloat"
    encodeFloat _ _ = error "CodeGenFunction Value: encodeFloat"
    exponent _ = 0
    scaleFloat 0 x = x
    scaleFloat _ _ = error "CodeGenFunction Value: scaleFloat"
    isNaN _ = error "CodeGenFunction Value: isNaN"
    isInfinite _ = error "CodeGenFunction Value: isInfinite"
    isDenormalized _ = error "CodeGenFunction Value: isDenormalized"
    isNegativeZero _ = error "CodeGenFunction Value: isNegativeZero"
    isIEEE _ = isIEEE (undefined :: a)

binop :: (Value a -> Value b -> TValue r c) ->
         TValue r a -> TValue r b -> TValue r c
binop op x y = do
    x' <- x
    y' <- y
    op x' y'

{-
If we add the ReadNone attribute, then LLVM-2.8 complains:

llvm/examples$ Arith_dyn.exe
Attribute readnone only applies to the function!
  %2 = call readnone double @llvm.sin.f64(double %0)
Attribute readnone only applies to the function!
  %3 = call readnone double @llvm.exp.f64(double %2)
Broken module found, compilation aborted!
Stack dump:
0.      Running pass 'Function Pass Manager' on module '_module'.
1.      Running pass 'Module Verifier' on function '@_fun1'
Aborted
-}
addReadNone :: Value a -> CodeGenFunction r (Value a)
addReadNone x = do
--   addAttributes x 0 [ReadNoneAttribute]
   return x

callIntrinsicP1 :: forall a b r . (IsFirstClass a, IsFirstClass b, IsPrimitive a) =>
                   String -> Value a -> TValue r b
callIntrinsicP1 fn x = do
    op :: Function (a -> IO b) <- externFunction ("llvm." ++ fn ++ "." ++ typeName (undefined :: a))
{-
You can add these attributes,
but the verifier pass in the optimizer checks whether they match
the attributes that are declared for that intrinsic.
If we omit adding attributes then the right attributes are added automatically.
    addFunctionAttributes op [NoUnwindAttribute, ReadOnlyAttribute]
-}
    call op x >>= addReadNone

callIntrinsicP2 :: forall a b c r . (IsFirstClass a, IsFirstClass b, IsFirstClass c, IsPrimitive a) =>
                   String -> Value a -> Value b -> TValue r c
callIntrinsicP2 fn x y = do
    op :: Function (a -> b -> IO c) <- externFunction ("llvm." ++ fn ++ "." ++ typeName (undefined :: a))
    call op x y >>= addReadNone

-------------------------------------------

class ArithFunction a b | a -> b, b -> a where
    arithFunction' :: a -> b

instance (Ret a r) => ArithFunction (CodeGenFunction r a) (CodeGenFunction r ()) where
    arithFunction' x = x >>= ret

instance (ArithFunction b b') => ArithFunction (CodeGenFunction r a -> b) (a -> b') where
    arithFunction' f = arithFunction' . f . return

-- |Unlift a function with @TValue@ to have @Value@ arguments.
arithFunction :: ArithFunction a b => a -> b
arithFunction = arithFunction'

-------------------------------------------

class UncurryN a b | a -> b, b -> a where
    uncurryN :: a -> b
    curryN :: b -> a

instance UncurryN (CodeGenFunction r a) (() -> CodeGenFunction r a) where
    uncurryN i = \ () -> i
    curryN f = f ()

instance (UncurryN t (b -> c)) => UncurryN (a -> t) ((a, b) -> c) where
    uncurryN f = \ (a, b) -> uncurryN (f a) b
    curryN f = \ a -> curryN (\ b -> f (a, b))

class LiftTuple r a b | a -> b, b -> a where
    liftTuple :: a -> CodeGenFunction r b

instance LiftTuple r () () where
    liftTuple = return

instance (LiftTuple r b b') => LiftTuple r (CodeGenFunction r a, b) (a, b') where
    liftTuple (a, b) = do a' <- a; b' <- liftTuple b; return (a', b')

class (UncurryN a (a1 -> CodeGenFunction r b1), LiftTuple r a1 b, UncurryN a2 (b -> CodeGenFunction r b1)) =>
      UnwrapArgs a a1 b1 b a2 r | a -> a1 b1, a1 b1 -> a, a1 -> b, b -> a1, a2 -> b b1, b b1 -> a2 where
    unwrapArgs :: a2 -> a
instance (UncurryN a (a1 -> CodeGenFunction r b1), LiftTuple r a1 b, UncurryN a2 (b -> CodeGenFunction r b1)) =>
         UnwrapArgs a a1 b1 b a2 r where
    unwrapArgs f = curryN $ \ x -> do x' <- liftTuple x; uncurryN f x'

-- |Lift a function from having @Value@ arguments to having @TValue@ arguments.
toArithFunction :: (CallArgs f g r, UnwrapArgs a a1 b1 b g r) =>
                    Function f -> a
toArithFunction f = unwrapArgs (call f)

-------------------------------------------

-- |Define a recursive 'arithFunction', gets passed itself as the first argument.
recursiveFunction ::
        (CallArgs a g r0,
         UnwrapArgs a11 a1 b1 b g r0,
         FunctionArgs a a2 r1,
         ArithFunction a3 a2,
         IsFunction a) =>
        (a11 -> a3) -> CodeGenModule (Function a)
recursiveFunction af = do
    f <- newFunction ExternalLinkage
    let f' = toArithFunction f
    defineFunction f $ arithFunction (af f')
    return f

-------------------------------------------

class CallIntrinsic a where
    callIntrinsic1' :: String -> Value a -> TValue r a
    callIntrinsic2' :: String -> Value a -> Value a -> TValue r a

instance CallIntrinsic Float where
    callIntrinsic1' = callIntrinsicP1
    callIntrinsic2' = callIntrinsicP2

instance CallIntrinsic Double where
    callIntrinsic1' = callIntrinsicP1
    callIntrinsic2' = callIntrinsicP2

{-
I think such a special case for certain systems
would be better handled as in LLVM.Extra.Extension.
(lemming)
-}
macOS :: Bool
#if defined(__MACOS__)
macOS = True
#else
macOS = False
#endif

instance (Pos n, IsPrimitive a, CallIntrinsic a) => CallIntrinsic (Vector n a) where
    callIntrinsic1' s x =
       if macOS && TypeNum.toInt (undefined :: n) == 4 &&
          elem s ["sqrt", "log", "exp", "sin", "cos", "tan"]
         then do
            op <- externFunction ("v" ++ s ++ "f")
            call op x >>= addReadNone
         else mapVector (callIntrinsic1' s) x
    callIntrinsic2' s = mapVector2 (callIntrinsic2' s)

callIntrinsic1 :: (CallIntrinsic a) => String -> TValue r a -> TValue r a
callIntrinsic1 s x = do x' <- x; callIntrinsic1' s x'

callIntrinsic2 :: (CallIntrinsic a) => String -> TValue r a -> TValue r a -> TValue r a
callIntrinsic2 s = binop (callIntrinsic2' s)
