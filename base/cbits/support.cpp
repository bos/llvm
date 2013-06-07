#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif
#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

#include "hs_llvm_config.h"

#include "llvm-c/Core.h"
#include "llvm/PassManager.h"
#include "llvm/Support/PrettyStackTrace.h"

#if HS_LLVM_VERSION < 303
# include "llvm/DefaultPasses.h"
#else
# include "llvm/CodeGen/Passes.h"
#endif
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/IPO.h"

#include "support.h"

using namespace llvm;

void LLVMCreateStandardFunctionPasses(LLVMPassManagerRef PM,
                                      unsigned OptimizationLevel)
{
  llvm::PassManagerBuilder Builder;
  Builder.OptLevel = OptimizationLevel;

  llvm::PassManagerBase *pass_man = unwrap(PM);
  llvm::FunctionPassManager *func_man =
    static_cast <FunctionPassManager*>(pass_man);

  if (func_man) {
    Builder.populateFunctionPassManager (*func_man);
  } else {
    // printf ("Cannot create function passes for module pass manager\n");
  }
}

void LLVMCreateStandardModulePasses(LLVMPassManagerRef PM,
                                    unsigned OptLevel,
                                    int OptimizeSize,
                                    int UnitAtATime,
                                    int UnrollLoops,
                                    int SimplifyLibCalls,
                                    int HaveExceptions,
                                    int DisableInline)
{
  llvm::PassManagerBuilder Builder;
  Builder.OptLevel = OptLevel;
  Builder.SizeLevel = OptimizeSize;
  Builder.DisableUnrollLoops = !UnrollLoops;
  Builder.DisableSimplifyLibCalls = !SimplifyLibCalls;
  Builder.DisableUnitAtATime = !UnitAtATime;

  Pass *InliningPass = 0;

  if (DisableInline) {
    // No inlining pass
  } else if (OptLevel) {
    unsigned Threshold = 225;
    if (OptLevel > 2)
      Threshold = 275;
    Builder.Inliner = createFunctionInliningPass(Threshold);
  } else {
    Builder.Inliner = createAlwaysInlinerPass();
  }

  Builder.populateModulePassManager (*unwrap(PM));
}

void LLVMDisablePrettyStackTrace()
{
  llvm::DisablePrettyStackTrace = true;
}

