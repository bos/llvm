module LLVM.Util.Optimize(optimizeModule) where
import Control.Monad
import Foreign.Ptr(nullPtr)

import LLVM.Core.Util(Module, withModule)
import qualified LLVM.FFI.Core as FFI
-- import LLVM.FFI.Target(addTargetData, createTargetData)
import LLVM.FFI.Transforms.IPO
import LLVM.FFI.Transforms.Scalar

optimizeModule :: Int -> Module -> IO Int
optimizeModule optLevel mdl = withModule mdl $ \ m -> do
    passes <- FFI.createPassManager

{-
Note on LLVM-2.6 to 2.8 (at least):
As far as I understand, if we do not set target data,
then the optimizer will only perform machine independent optimizations.
If we set target data
(e.g. an empty layout string obtained from a module without 'target data' specification.)
we risk that the optimizer switches to a wrong layout
(e.g. to 64 bit pointers on a 32 bit machine for empty layout string)
and thus generates corrupt code.

Currently it seems to be safer to disable
machine dependent optimization completely.

http://llvm.org/bugs/show_bug.cgi?id=6394

    -- Pass the module target data to the pass manager.
    target <- FFI.getDataLayout m >>= createTargetData
    addTargetData target passes
-}

--FCN    fPasses <- FFI.createFunctionPassManager mp
    let fPasses = nullPtr
    -- XXX add module target data

--    addVerifierPass passes -- XXX does not exist
    addLowerSetJmpPass passes
    addOptimizationPasses passes fPasses optLevel

    --FCN XXX loop through all functions and optimize them.
--    initializeFunctionPassManager fPasses
--    runFunctionPassManager fPasses fcn

--    addVerifierPass passes -- XXX does not exist

    rc <- FFI.runPassManager passes m
    -- XXX discard pass manager?

    return (fromIntegral rc)

addOptimizationPasses :: FFI.PassManagerRef -> FFI.PassManagerRef -> Int -> IO ()
addOptimizationPasses passes fPasses optLevel = do
    createStandardFunctionPasses fPasses optLevel

    let inline = addFunctionInliningPass --  if optLevel > 1 then addFunctionInliningPass else const (return ())
    createStandardModulePasses passes optLevel True (optLevel > 1) True True inline

createStandardFunctionPasses :: FFI.PassManagerRef -> Int -> IO ()
createStandardFunctionPasses fPasses optLevel = do
  when False $ do -- FCN
    addCFGSimplificationPass fPasses
    if optLevel == 1 then
        addPromoteMemoryToRegisterPass fPasses
     else
        addScalarReplAggregatesPass fPasses
    addInstructionCombiningPass fPasses

createStandardModulePasses :: FFI.PassManagerRef -> Int -> Bool -> Bool -> Bool -> Bool -> (FFI.PassManagerRef -> IO()) -> IO ()
createStandardModulePasses passes optLevel unitAtATime unrollLoops simplifyLibCalls haveExceptions inliningPass = do
    when unitAtATime $ do
        addRaiseAllocationsPass passes
    addCFGSimplificationPass passes
    addPromoteMemoryToRegisterPass passes
    when unitAtATime $ do
        addGlobalOptimizerPass passes
        addGlobalDCEPass passes
        addIPConstantPropagationPass passes
        addDeadArgEliminationPass passes
    addInstructionCombiningPass passes
    addCFGSimplificationPass passes
    when unitAtATime $ do
        when haveExceptions $ addPruneEHPass passes
        addFunctionAttrsPass passes
    inliningPass passes
    when (optLevel > 2) $ do
        addArgumentPromotionPass passes
    when simplifyLibCalls $ do
        addSimplifyLibCallsPass passes
    addInstructionCombiningPass passes
    addJumpThreadingPass passes
    addCFGSimplificationPass passes
    addScalarReplAggregatesPass passes
    addInstructionCombiningPass passes
    addTailCallEliminationPass passes
    addCFGSimplificationPass passes
    addReassociatePass passes
    addLoopRotatePass passes
    addLICMPass passes
    addLoopUnswitchPass passes
    addInstructionCombiningPass passes
    addIndVarSimplifyPass passes
    addLoopDeletionPass passes
    when unrollLoops $
      addLoopUnrollPass passes
    addInstructionCombiningPass passes
    addGVNPass passes
    addMemCpyOptPass passes
    addSCCPPass passes

    addInstructionCombiningPass passes
    addDeadStoreEliminationPass passes
    addAggressiveDCEPass passes
    addCFGSimplificationPass passes

    when unitAtATime $ do
      addStripDeadPrototypesPass passes
      addDeadTypeEliminationPass passes

    when (optLevel > 1 && unitAtATime) $
      addConstantMergePass passes
