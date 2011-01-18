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

import Control.Monad(when)

import LLVM.Core.Util(Module, withModule)
import qualified LLVM.FFI.Core as FFI
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
    -- XXX discard pass manager?

    return $
       toEnum (fromIntegral moduleModified) ||
       toEnum (fromIntegral functionsModified)

-- tools/opt/opt.cpp: AddOptimizationPasses
addOptimizationPasses :: FFI.PassManagerRef -> FFI.PassManagerRef -> Int -> IO ()
addOptimizationPasses passes fPasses optLevel = do
    createStandardFunctionPasses fPasses optLevel

    -- if optLevel > 1 then addFunctionInliningPass else const (return ())
    let inline = addFunctionInliningPass
    createStandardModulePasses passes optLevel True (optLevel > 1) True True inline

-- llvm/Support/StandardPasses.h: createStandardFunctionPasses
createStandardFunctionPasses :: FFI.PassManagerRef -> Int -> IO ()
createStandardFunctionPasses fPasses optLevel = do
  when (optLevel > 0) $ do
    addCFGSimplificationPass fPasses
    if optLevel == 1
      then addPromoteMemoryToRegisterPass fPasses
      else addScalarReplAggregatesPass fPasses
    addInstructionCombiningPass fPasses

-- llvm/Support/StandardPasses.h: createStandardModulePasses
createStandardModulePasses :: FFI.PassManagerRef -> Int -> Bool -> Bool -> Bool -> Bool -> (FFI.PassManagerRef -> IO()) -> IO ()
createStandardModulePasses passes optLevel unitAtATime unrollLoops simplifyLibCalls haveExceptions inliningPass = do
  if optLevel == 0
    then inliningPass passes
    else do
      when unitAtATime $ do
        addGlobalOptimizerPass passes     -- Optimize out global vars

        addIPSCCPPass passes              -- IP SCCP
        addDeadArgEliminationPass passes  -- Dead argument elimination

      addInstructionCombiningPass passes  -- Clean up after IPCP & DAE
      addCFGSimplificationPass passes     -- Clean up after IPCP & DAE

      -- Start of CallGraph SCC passes.
      when (unitAtATime && haveExceptions) $
        addPruneEHPass passes             -- Remove dead EH info
      inliningPass passes
      when unitAtATime $
        addFunctionAttrsPass passes       -- Set readonly/readnone attrs
      when (optLevel > 2) $
        addArgumentPromotionPass passes   -- Scalarize uninlined fn args

      -- Start of function pass.
      addScalarReplAggregatesPass passes  -- Break up aggregate allocas
      when simplifyLibCalls $
        addSimplifyLibCallsPass passes    -- Library Call Optimizations
      addInstructionCombiningPass passes  -- Cleanup for scalarrepl.
      addJumpThreadingPass passes         -- Thread jumps.
      addCFGSimplificationPass passes     -- Merge & remove BBs
      addInstructionCombiningPass passes  -- Combine silly seq's

      addTailCallEliminationPass passes   -- Eliminate tail calls
      addCFGSimplificationPass passes     -- Merge & remove BBs
      addReassociatePass passes           -- Reassociate expressions
      addLoopRotatePass passes            -- Rotate Loop
      addLICMPass passes                  -- Hoist loop invariants
      -- The C interface does not allow to pass the optimizeForSize parameter
      -- addLoopUnswitchPass(optimizeSize || optLevel < 3));
      addInstructionCombiningPass passes
      addIndVarSimplifyPass passes        -- Canonicalize indvars
      addLoopDeletionPass passes          -- Delete dead loops
      when unrollLoops $
        addLoopUnrollPass passes          -- Unroll small loops
      addInstructionCombiningPass passes  -- Clean up after the unroller
      when (optLevel > 1) $
        addGVNPass passes                 -- Remove redundancies
      addMemCpyOptPass passes             -- Remove memcpy / form memset
      addSCCPPass passes                  -- Constant prop with SCCP

      -- Run instcombine after redundancy elimination to exploit opportunities
      -- opened up by them.
      addInstructionCombiningPass passes
      addJumpThreadingPass passes         -- Thread jumps
      -- Not available in C interface
      -- addCorrelatedValuePropagationPass
      addDeadStoreEliminationPass passes  -- Delete dead stores
      addAggressiveDCEPass passes         -- Delete dead instructions
      addCFGSimplificationPass passes     -- Merge & remove BBs

      when unitAtATime $ do
        addStripDeadPrototypesPass passes -- Get rid of dead prototypes
        addDeadTypeEliminationPass passes -- Eliminate dead types

        -- GlobalOpt already deletes dead functions and globals, at -O3 try a
        -- late pass of GlobalDCE.  It is capable of deleting dead cycles.
        when (optLevel > 2) $
          addGlobalDCEPass passes         -- Remove dead fns and globals.

        when (optLevel > 1) $
          addConstantMergePass passes     -- Merge dup global constants


{-
ToDo:
Function that adds passes according to a list of opt-options.
This would simplify to get consistent behaviour between opt and optimizeModule.

-verify                    addVerifierPass
                           addLowerSetJmpPass
-globalopt                 addGlobalOptimizerPass
-ipsccp                    addIPSCCPPass
-deadargelim               addDeadArgEliminationPass
-instcombine               addInstructionCombiningPass
-simplifycfg               addCFGSimplificationPass
-prune-eh                  addPruneEHPass
-functionattrs             addFunctionAttrsPass
-scalarrepl                addScalarReplAggregatesPass
-simplify-libcalls         addSimplifyLibCallsPass
-instcombine               addInstructionCombiningPass
-jump-threading            addJumpThreadingPass
-simplifycfg               addCFGSimplificationPass
-instcombine               addInstructionCombiningPass
-tailcallelim              addTailCallEliminationPass
-simplifycfg               addCFGSimplificationPass
-reassociate               addReassociatePass
-loop-rotate               addLoopRotatePass
-licm                      addLICMPass
-instcombine               addInstructionCombiningPass
-indvars                   addIndVarSimplifyPass
-loop-deletion             addLoopDeletionPass
-instcombine               addInstructionCombiningPass
-memcpyopt                 addMemCpyOptPass
-sccp                      addSCCPPass
-instcombine               addInstructionCombiningPass
-jump-threading            addJumpThreadingPass
-dse                       addDeadStoreEliminationPass
-adce                      addAggressiveDCEPass
-simplifycfg               addCFGSimplificationPass
-strip-dead-prototypes     addStripDeadPrototypesPass
-deadtypeelim              addDeadTypeEliminationPass
-}
