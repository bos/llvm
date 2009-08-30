{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}
 -- |An 'ExecutionEngine' is JIT compiler that is used to generate code for an LLVM module.
module LLVM.ExecutionEngine(
    -- * Execution engine
    EngineAccess,
    runEngineAccess,
    addModuleProvider,
    addModule,
{-
    runStaticConstructors,
    runStaticDestructors,
-}
    getPointerToFunction,
    getFreePointers, FreePointers,
    -- * Translation
    Translatable, Generic,
    generateFunction,
    -- * Unsafe type conversion
    Unsafe,
    unsafePurify,
    -- * Simplified interface.
    simpleFunction,
    unsafeGenerateFunction,
    -- * Target information
    module LLVM.ExecutionEngine.Target
    ) where
import System.IO.Unsafe (unsafePerformIO)

import LLVM.ExecutionEngine.Engine
import LLVM.FFI.Core(ValueRef)
import LLVM.Core.CodeGen(Value(..))
import LLVM.Core
import LLVM.ExecutionEngine.Target
--import LLVM.Core.Util(runFunctionPassManager, initializeFunctionPassManager, finalizeFunctionPassManager)

-- |Class of LLVM function types that can be translated to the corresponding
-- Haskell type.
class Translatable f where
    translate :: (ValueRef -> [GenericValue] -> IO GenericValue) -> [GenericValue] -> ValueRef -> f

instance (Generic a, Translatable b) => Translatable (a -> b) where
    translate run args f = \ arg -> translate run (toGeneric arg : args) f

instance (Generic a) => Translatable (IO a) where
    translate run args f = fmap fromGeneric $ run f $ reverse args

-- |Generate a Haskell function from an LLVM function.
generateFunction :: (Translatable f) =>
                    Value (Ptr f) -> EngineAccess f
generateFunction (Value f) = do
    run <- getRunFunction
    return $ translate run [] f

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
    prov <- createModuleProviderForExistingModule m
    runEngineAccess $ do
        addModuleProvider prov
        generateFunction func

{-
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
-}

-- | Combine 'simpleFunction' and 'unsafePurify'.
unsafeGenerateFunction :: (Unsafe t a, Translatable t) =>
                          CodeGenModule (Function t) -> a
unsafeGenerateFunction bld = unsafePerformIO $ do
    fun <- simpleFunction bld
    return $ unsafePurify fun
