{-
LLVM does not export its functions
@createStandardFunctionPasses@ and
@createStandardModulePasses@ via its C interface
and interfacing to C-C++ wrappers is not very portable.
Thus we reimplement these functions
from @opt.cpp@ and @StandardPasses.h@ in Haskell.
However this way we risk inconsistencies
between 'optimizeModule' and the @opt@ shell command.
-}
module LLVM.Util.Optimize(optimizeModule) where

import LLVM.Core.Util(Module, withModule)
import qualified LLVM.FFI.Core as FFI
import qualified LLVM.FFI.Support as FFI
-- import LLVM.FFI.Target(addTargetData, createTargetData)
import LLVM.FFI.Transforms.IPO
import LLVM.FFI.Transforms.Scalar
import Control.Exception (bracket, )


{- |
Result tells whether the module was modified by any of the passes.
-}
optimizeModule :: Int -> Module -> IO Bool
optimizeModule optLevel mdl =
    withModule mdl $ \ m ->
    {-
    Core.Util.createPassManager would provide a finalizer for us,
    but I think it is better here to immediately dispose the manager
    when we need it no longer.
    -}
    bracket FFI.createPassManager FFI.disposePassManager $ \ passes ->

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

    {-
    opt.cpp does not use a FunctionPassManager for function optimization,
    but a module PassManager.
    Thus we do it the same way.
    I assume that we would need a FunctionPassManager
    only if we wanted to apply individual optimizations to functions.

    fPasses <- FFI.createFunctionPassManager mp
    -}
    bracket FFI.createPassManager FFI.disposePassManager $ \ fPasses -> do
    -- add module target data?

    -- tools/opt/opt.cpp: AddStandardCompilePasses
    addVerifierPass passes
    addLowerSetJmpPass passes
    addOptimizationPasses passes fPasses optLevel

    {- if we wanted to do so, we could loop through all functions and optimize them.
    initializeFunctionPassManager fPasses
    runFunctionPassManager fPasses fcn
    -}

    functionsModified <- FFI.runPassManager fPasses m

    moduleModified <- FFI.runPassManager passes m

    return $
       toEnum (fromIntegral moduleModified) ||
       toEnum (fromIntegral functionsModified)

-- tools/opt/opt.cpp: AddOptimizationPasses
addOptimizationPasses :: FFI.PassManagerRef -> FFI.PassManagerRef -> Int -> IO ()
addOptimizationPasses passes fPasses optLevel = do
  createStandardFunctionPasses fPasses optLevel
  createStandardModulePasses passes optLevel True True (optLevel > 1) True True True

createStandardFunctionPasses :: FFI.PassManagerRef -> Int -> IO ()
createStandardFunctionPasses fPasses optLevel =
    FFI.createStandardFunctionPasses fPasses (fromIntegral optLevel)

-- llvm/Support/StandardPasses.h: createStandardModulePasses
createStandardModulePasses :: FFI.PassManagerRef -> Int -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> IO ()
createStandardModulePasses passes optLevel optSize unitAtATime unrollLoops simplifyLibCalls haveExceptions inliningPass =
  FFI.createStandardModulePasses passes (fromIntegral optLevel) (f optSize)
     (f unitAtATime) (f unrollLoops) (f simplifyLibCalls) (f haveExceptions)
     (f (not inliningPass))
  where f True = 1
        f _    = 0


{-
ToDo:
Function that adds passes according to a list of opt-options.
This would simplify to get consistent behaviour between opt and optimizeModule.

-adce                      addAggressiveDCEPass
-deadargelim               addDeadArgEliminationPass
-deadtypeelim              addDeadTypeEliminationPass
-dse                       addDeadStoreEliminationPass
-functionattrs             addFunctionAttrsPass
-globalopt                 addGlobalOptimizerPass
-indvars                   addIndVarSimplifyPass
-instcombine               addInstructionCombiningPass
-ipsccp                    addIPSCCPPass
-jump-threading            addJumpThreadingPass
-licm                      addLICMPass
-loop-deletion             addLoopDeletionPass
-loop-rotate               addLoopRotatePass
-lowersetjmp               addLowerSetJmpPass
-memcpyopt                 addMemCpyOptPass
-prune-eh                  addPruneEHPass
-reassociate               addReassociatePass
-scalarrepl                addScalarReplAggregatesPass
-sccp                      addSCCPPass
-simplifycfg               addCFGSimplificationPass
-simplify-libcalls         addSimplifyLibCallsPass
-strip-dead-prototypes     addStripDeadPrototypesPass
-tailcallelim              addTailCallEliminationPass
-verify                    addVerifierPass
-}
