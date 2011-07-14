#ifndef LLVM_HS_SUPPORT_H
#define LLVM_HS_SUPPORT_H

#ifdef __cplusplus
extern "C" {
#endif

void LLVMCreateStandardFunctionPasses(LLVMPassManagerRef PM,
				      unsigned OptimizationLevel);


#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* LLVM_HS_SUPPORT_H */
