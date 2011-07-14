#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#include "llvm-c/Core.h"
#include "llvm/PassManager.h"
#include "llvm/Support/StandardPasses.h"

#include "support.h"

void LLVMCreateStandardFunctionPasses(LLVMPassManagerRef PM,
				      unsigned OptimizationLevel)
{
  llvm::createStandardFunctionPasses(llvm::unwrap(PM), OptimizationLevel);
}
