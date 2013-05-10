#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif
#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>

/* C function to free function object resources.  Can be called from a finalizer. */
void
c_freeFunctionObject(LLVMExecutionEngineRef execEngine,
		     LLVMModuleProviderRef moduleProvider,
		     LLVMValueRef f)
{
  LLVMModuleRef mod;
  LLVMFreeMachineCodeForFunction(execEngine, f);
  if (!LLVMRemoveModuleProvider(execEngine, moduleProvider, &mod, 0)) {
    LLVMDisposeModule(mod);
  }
}
