{-# LANGUAGE CPP, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}
 -- |An 'ExecutionEngine' is JIT compiler that is used to generate code for an LLVM module.
module LLVM.ExecutionEngine(
    -- * Execution engine
    ExecutionEngine,
    createExecutionEngine,
    addModuleProvider,
    runStaticConstructors,
    runStaticDestructors,
#if HAS_GETPOINTERTOGLOBAL
    getPointerToFunction,
#endif
    -- * Translation
    Translatable, Generic,
    generateFunction,
    -- * Unsafe type conversion
    Unsafe,
    unsafePurify,
    -- * Simplified interface.
    simpleFunction,
    unsafeGenerateFunction
    ) where
import System.IO.Unsafe (unsafePerformIO)

import LLVM.ExecutionEngine.Engine
import LLVM.FFI.Core(ValueRef)
import LLVM.Core.CodeGen(Value(..))
import LLVM.Core
import LLVM.Core.Util(runFunctionPassManager, initializeFunctionPassManager, finalizeFunctionPassManager)

-- |Class of LLVM function types that can be translated to the corresponding
-- Haskell type.
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

class Unsafe a b | a -> b where
    unsafePurify :: a -> b  -- ^Remove the IO from a function return type.  This is unsafe in general.

instance (Unsafe b b') => Unsafe (a->b) (a->b') where
    unsafePurify f = unsafePurify . f

instance Unsafe (IO a) a where
    unsafePurify = unsafePerformIO

-- |Translate a function to Haskell code.  This is a simplified interface to
-- the execution engine and module mechanism.
simpleFunction :: (Translatable f) => CodeGenModule (Function f) -> IO f
simpleFunction bld = do
    m <- newModule
    func <- defineModule m bld
--    dumpValue func
    prov <- createModuleProviderForExistingModule m
    ee <- createExecutionEngine prov

    pm <- createFunctionPassManager prov
    td <- getExecutionEngineTargetData ee
    addTargetData td pm
    addInstructionCombiningPass pm
    addReassociatePass pm
    addGVNPass pm
    addCFGSimplificationPass pm
    addPromoteMemoryToRegisterPass pm
    initializeFunctionPassManager pm
--    print ("rc1", rc1)
    runFunctionPassManager pm (unValue func)
--    print ("rc2", rc2)
    finalizeFunctionPassManager pm
--    print ("rc3", rc3)
--    dumpValue func

    return $ generateFunction ee func

-- | Combine 'simpleFunction' and 'unsafePurify'.
unsafeGenerateFunction :: (Unsafe t a, Translatable t) =>
                          CodeGenModule (Function t) -> a
unsafeGenerateFunction bld = unsafePerformIO $ do
    fun <- simpleFunction bld
    return $ unsafePurify fun
