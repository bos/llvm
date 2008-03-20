{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module LLVM.ExecutionEngine(
    -- * Execution engine
    ExecutionEngine,
    createExecutionEngine,
    runStaticConstructors,
    runStaticDestructors,
    -- * Translation
    Translatable, Generic,
    generateFunction,
    unsafePurify
    ) where
import System.IO.Unsafe (unsafePerformIO)

import LLVM.ExecutionEngine.Engine
import LLVM.Core.FFI(ValueRef)
import LLVM.Core.Data(Ptr)
import LLVM.Core.CodeGen(Value(..))

class Translatable f where
    translate :: ExecutionEngine -> [GenericValue] -> ValueRef -> f

instance (Generic a, Translatable b) => Translatable (a -> b) where
    translate ee args f = \ arg -> translate ee (toGeneric arg : args) f

instance (Generic a) => Translatable (IO a) where
    translate ee args f = fmap fromGeneric $ runFunction ee f $ reverse args

-- |Generate a Haskell function from an LLVM function.
generateFunction :: (Translatable f) =>
                    ExecutionEngine -> Value (Ptr f) -> f
generateFunction ee (Value f) = translate ee [] f

-- |Remove the IO from a function return type.  This is unsafe in general.
class Unsafe a b | a -> b where
    unsafePurify :: a -> b

instance (Unsafe b b') => Unsafe (a->b) (a->b') where
    unsafePurify f = unsafePurify . f

instance Unsafe (IO a) a where
    unsafePurify = unsafePerformIO
