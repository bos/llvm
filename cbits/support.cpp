#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#include "llvm-c/Core.h"
#include "llvm/PassManager.h"
#include "llvm/Support/StandardPasses.h"

#include "support.h"

using namespace llvm;

void LLVMCreateStandardFunctionPasses(LLVMPassManagerRef PM,
				      unsigned OptimizationLevel)
{
  createStandardFunctionPasses(unwrap(PM), OptimizationLevel);
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
}
