{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, TypeOperators #-}
module Loop(Phi, (:*)(..), forLoop) where
import LLVM.Core

class Phi a where
    phis :: BasicBlock -> a -> CodeGenFunction r a
    addPhis :: BasicBlock -> a -> a -> CodeGenFunction r ()

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

instance Phi () where
    phis _ _ = return ()
    addPhis _ _ _ = return ()

instance (IsFirstClass a) => Phi (Value a) where
    phis bb a = do
        a' <- phi [(a, bb)]
        return a'
    addPhis bb a a' = do
        addPhiInputs a [(a', bb)]

instance (IsFirstClass a, IsFirstClass b) => Phi (Value a, Value b) where
    phis bb (a, b) = do
        a' <- phi [(a, bb)]
        b' <- phi [(b, bb)]
        return (a', b')
    addPhis bb (a, b) (a', b') = do
        addPhiInputs a [(a', bb)]
        addPhiInputs b [(b', bb)]

instance (IsFirstClass a, IsFirstClass b, IsFirstClass c) => Phi (Value a, Value b, Value c) where
    phis bb (a, b, c) = do
        a' <- phi [(a, bb)]
        b' <- phi [(b, bb)]
        c' <- phi [(c, bb)]
        return (a', b', c')
    addPhis bb (a, b, c) (a', b', c') = do
        addPhiInputs a [(a', bb)]
        addPhiInputs b [(b', bb)]
        addPhiInputs c [(c', bb)]

-- Loop the index variable from low to high.  The state in the loop starts as start, and is modified
-- by incr in each iteration.  The loop index is treated as unsigned.
forLoop :: forall i a r . (Phi a, Num i, IsConst i, IsInteger i, IsFirstClass i) =>
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
    t <- icmp IntULT i high
    condBr t body exit

    defineBasicBlock body

    vars' <- incr i vars
    i' <- add i (valueOf 1 :: Value i)

    addPhis body vars vars'
    addPhiInputs i [(i', body)]
    br loop
    defineBasicBlock exit

    return vars