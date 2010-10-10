{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, TypeOperators, FlexibleContexts #-}
module LLVM.Util.Loop(Phi(phis,addPhis), forLoop, mapVector, mapVector2) where
import Data.TypeLevel hiding (Bool)
import LLVM.Core

class Phi a where
    phis :: BasicBlock -> a -> CodeGenFunction r a
    addPhis :: BasicBlock -> a -> a -> CodeGenFunction r ()

{-
infixr 1 :*
-- XXX should use HList if it was packaged in a nice way.
data a :* b = a :* b
    deriving (Eq, Ord, Show, Read)

instance (IsFirstClass a, Phi b) => Phi (Value a :* b) where
    phis bb (a :* b) = do
        a' <- phi [(a, bb)]
        b' <- phis bb b
        return (a' :* b')
    addPhis bb (a :* b) (a' :* b') = do
        addPhiInputs a [(a', bb)]
        addPhis bb b b'
-}

instance Phi () where
    phis _ _ = return ()
    addPhis _ _ _ = return ()

instance (IsFirstClass a) => Phi (Value a) where
    phis bb a = do
        a' <- phi [(a, bb)]
        return a'
    addPhis bb a a' = do
        addPhiInputs a [(a', bb)]

instance (Phi a, Phi b) => Phi (a, b) where
    phis bb (a, b) = do
        a' <- phis bb a
        b' <- phis bb b
        return (a', b')
    addPhis bb (a, b) (a', b') = do
        addPhis bb a a'
        addPhis bb b b'

instance (Phi a, Phi b, Phi c) => Phi (a, b, c) where
    phis bb (a, b, c) = do
        a' <- phis bb a
        b' <- phis bb b
        c' <- phis bb c
        return (a', b', c')
    addPhis bb (a, b, c) (a', b', c') = do
        addPhis bb a a'
        addPhis bb b b'
        addPhis bb c c'

-- Loop the index variable from low to high.  The state in the loop starts as start, and is modified
-- by incr in each iteration.
forLoop :: forall i a r . (Phi a, Num i, IsConst i, IsInteger i, IsFirstClass i, CmpRet i Bool) =>
           Value i -> Value i -> a -> (Value i -> a -> CodeGenFunction r a) -> CodeGenFunction r a
forLoop low high start incr = do
    top <- getCurrentBasicBlock
    loop <- newBasicBlock
    body <- newBasicBlock
    exit <- newBasicBlock

    br loop

    defineBasicBlock loop
    i <- phi [(low, top)]
    vars <- phis top start
    t <- icmp IntNE i high
    condBr t body exit

    defineBasicBlock body

    vars' <- incr i vars
    i' <- add i (valueOf 1 :: Value i)

    body' <- getCurrentBasicBlock
    addPhis body' vars vars'
    addPhiInputs i [(i', body')]
    br loop
    defineBasicBlock exit

    return vars

--------------------------------------

mapVector :: forall a b n r .
             (Pos n, IsPrimitive b) =>
             (Value a -> CodeGenFunction r (Value b)) ->
             Value (Vector n a) -> CodeGenFunction r (Value (Vector n b))
mapVector f v =
    forLoop (valueOf 0) (valueOf (toNum (undefined :: n))) (value undef) $ \ i w -> do
        x <- extractelement v i
        y <- f x
        insertelement w y i

mapVector2 :: forall a b c n r .
             (Pos n, IsPrimitive c) =>
             (Value a -> Value b -> CodeGenFunction r (Value c)) ->
             Value (Vector n a) -> Value (Vector n b) -> CodeGenFunction r (Value (Vector n c))
mapVector2 f v1 v2 =
    forLoop (valueOf 0) (valueOf (toNum (undefined :: n))) (value undef) $ \ i w -> do
        x <- extractelement v1 i
        y <- extractelement v2 i
        z <- f x y
        insertelement w z i
