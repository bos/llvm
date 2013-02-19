#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#include "hs_llvm_config.h"

#include "llvm-c/Core.h"
#include "llvm/PassManager.h"
#if HS_LLVM_VERSION >= 300
# include "llvm/DefaultPasses.h"
# include "llvm/Transforms/IPO/PassManagerBuilder.h"
# include "llvm/Transforms/IPO.h"
#else
# include "llvm/Support/StandardPasses.h"
#endif

#include "support.h"

using namespace llvm;

void LLVMCreateStandardFunctionPasses(LLVMPassManagerRef PM,
					unsigned OptimizationLevel)
{
#if HS_LLVM_VERSION >= 300
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
#else
  createStandardFunctionPasses(unwrap(PM), OptimizationLevel);
#endif
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
#if HS_LLVM_VERSION >= 300
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
#else
  Pass *InliningPass = 0;

  if (DisableInline) {
    // No inlining pass
  } else if (OptLevel) {
    unsigned Threshold = 225;
    if (OptLevel > 2)
      Threshold = 275;
    InliningPass = createFunctionInliningPass(Threshold);
  } else {
    InliningPass = createAlwaysInlinerPass();
  }

  createStandardModulePasses(unwrap(PM), OptLevel, OptimizeSize,
                             UnitAtATime, UnrollLoops, SimplifyLibCalls,
                             HaveExceptions, InliningPass);
#endif
}
