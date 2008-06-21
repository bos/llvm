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
    unsafePurify,
    simpleFunction,
    unsafeGenerateFunction
    ) where
import System.IO.Unsafe (unsafePerformIO)

import LLVM.ExecutionEngine.Engine
import LLVM.Core.FFI(ValueRef)
import LLVM.Core.Data(Ptr)
import LLVM.Core.CodeGen(Value(..))
import LLVM.Core

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

-- |Translate a function to Haskell code.
simpleFunction :: (Translatable f) => CodeGenModule (Function f) -> IO f
simpleFunction bld = do
    m <- newModule
    func <- defineModule m bld
    prov <- createModuleProviderForExistingModule m
    ee <- createExecutionEngine prov
    return $ generateFunction ee func

unsafeGenerateFunction :: (Unsafe t a, Translatable t) =>
                          CodeGenModule (Function t) -> a
unsafeGenerateFunction bld = unsafePerformIO $ do
    fun <- simpleFunction bld
    return $ unsafePurify fun

