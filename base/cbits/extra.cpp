/*
 * Copyright (c) 2008-10, Mahadevan R All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 *  * Neither the name of this software, nor the names of its 
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/**
 * These are some "extra" functions not available in the standard LLVM-C
 * bindings, but are required / good-to-have inorder to implement the
 * Python bindings.
 */

#include "hs_llvm_config.h"

// standard includes
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <sstream>

#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

// LLVM includes
#if HS_LLVM_VERSION < 303
#include "llvm/LLVMContext.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/GlobalVariable.h"
#include "llvm/IntrinsicInst.h"
#else
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IntrinsicInst.h"
#endif
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Casting.h"
#if HS_LLVM_VERSION < 300
#include "llvm/TypeSymbolTable.h"
#endif
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/CallSite.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Assembly/Parser.h"
#ifdef HAVE_LLVM_SUPPORT_DYNAMICLIBRARY_H
# include "llvm/Support/DynamicLibrary.h"
#else
# include "llvm/System/DynamicLibrary.h"
#endif
#include "llvm/PassManager.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/DomPrinter.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"
#include "llvm/Transforms/Instrumentation.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Linker.h"
#include "llvm/Support/SourceMgr.h"

#if HS_LLVM_VERSION >= 300
// Imports for direct object emission
// Target selection
#if HS_LLVM_VERSION < 302
#include "llvm/Target/TargetData.h"
#elif HS_LLVM_VERSION < 303
#include "llvm/DataLayout.h"
#else
#include "llvm/IR/DataLayout.h"
#endif
#include "llvm/Target/TargetMachine.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/Host.h"
#include "llvm/ADT/SmallVector.h"

// File output
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FormattedStream.h"
#endif

// LLVM-C includes
#include "llvm-c/Core.h"
#include "llvm-c/ExecutionEngine.h"
#include "llvm-c/Target.h"

// our includes
#include "extra.h"

//using namespace llvm;

unsigned LLVMInitNativeTarget()
{
    return LLVMInitializeNativeTarget();
}

char *LLVMDumpModuleToString(LLVMModuleRef module)
{
    std::string s;
    llvm::raw_string_ostream buf(s);
    llvm::Module *p = llvm::unwrap(module);
    assert(p);
    p->print(buf, NULL);
    return strdup(buf.str().c_str());
}

char *LLVMDumpTypeToString(LLVMTypeRef type)
{
    std::string s;
    llvm::raw_string_ostream buf(s);
    llvm::Type *p = llvm::unwrap(type);
    assert(p);
    p->print(buf);
    return strdup(buf.str().c_str());
}

char *LLVMDumpValueToString(LLVMValueRef value)
{
    std::string s;
    llvm::raw_string_ostream buf(s);
    llvm::Value *p = llvm::unwrap(value);
    assert(p);
    p->print(buf);
    return strdup(buf.str().c_str());
}

unsigned LLVMModuleGetPointerSize(LLVMModuleRef module)
{
    llvm::Module *modulep = llvm::unwrap(module);
    assert(modulep);

    llvm::Module::PointerSize p = modulep->getPointerSize();
    if (p == llvm::Module::Pointer32)
        return 32;
    else if (p == llvm::Module::Pointer64)
        return 64;
    return 0;
}

LLVMValueRef LLVMModuleGetOrInsertFunction(LLVMModuleRef module,
    const char *name, LLVMTypeRef function_type)
{
    assert(name);

    llvm::Module *modulep = llvm::unwrap(module);
    assert(modulep);

    llvm::FunctionType *ftp = llvm::unwrap<llvm::FunctionType>(function_type);
    assert(ftp);

    llvm::Constant *f = modulep->getOrInsertFunction(name, ftp);
    return wrap(f);
}

int LLVMHasInitializer(LLVMValueRef global_var)
{
    llvm::GlobalVariable *gvp = llvm::unwrap<llvm::GlobalVariable>(global_var);
    assert(gvp);

    return gvp->hasInitializer();
}

#define inst_checkfn(ourfn, llvmfn)                 \
unsigned ourfn (LLVMValueRef v) {                   \
    llvm::Instruction *ip = llvm::unwrap<llvm::Instruction>(v); \
    assert(ip);                                     \
    return ip-> llvmfn () ? 1 : 0;                  \
}

inst_checkfn(LLVMInstIsTerminator,      isTerminator)
inst_checkfn(LLVMInstIsBinaryOp,        isBinaryOp)
inst_checkfn(LLVMInstIsShift,           isShift)
inst_checkfn(LLVMInstIsCast,            isCast)
inst_checkfn(LLVMInstIsLogicalShift,    isLogicalShift)
inst_checkfn(LLVMInstIsArithmeticShift, isArithmeticShift)
inst_checkfn(LLVMInstIsAssociative,     isAssociative)
inst_checkfn(LLVMInstIsCommutative,     isCommutative)

unsigned LLVMInstIsVolatile(LLVMValueRef v)
{
    using namespace llvm;
    Instruction *ip = unwrap<Instruction>(v);
    assert(ip);
    return ((isa<LoadInst>(*ip)  && cast<LoadInst>(*ip).isVolatile()) ||
            (isa<StoreInst>(*ip) && cast<StoreInst>(*ip).isVolatile()) );
}

const char *LLVMInstGetOpcodeName(LLVMValueRef inst)
{
    llvm::Instruction *instp = llvm::unwrap<llvm::Instruction>(inst);
    assert(instp);
    return instp->getOpcodeName();
}

#if HS_LLVM_VERSION < 301
unsigned LLVMInstGetOpcode(LLVMValueRef inst)
{
    llvm::Instruction *instp = llvm::unwrap<llvm::Instruction>(inst);
    assert(instp);
    return instp->getOpcode();
}
#endif

unsigned LLVMCmpInstGetPredicate(LLVMValueRef cmpinst)
{
    llvm::CmpInst *instp = llvm::unwrap<llvm::CmpInst>(cmpinst);
    assert(instp);
    return instp->getPredicate();
}

/* llvm::unwrap a set of `n' wrapped objects starting at `values',
 * into a vector of pointers to llvm::unwrapped objects `out'. */
template <typename W, typename UW>
void unwrap_vec(W *values, unsigned n, std::vector<UW *>& out)
{
    out.clear();

    while (n--) {
        UW *p = llvm::unwrap(*values);
        assert(p);
        out.push_back(p);
        ++values;
    }
}

/* Same as llvm::unwrap_vec, but use a vector of const pointers. */
template <typename W, typename UW>
void unwrap_cvec(W *values, unsigned n, std::vector<const UW *>& out)
{
    out.clear();

    while (n--) {
        UW *p = llvm::unwrap(*values);
        assert(p);
        out.push_back(p);
        ++values;
    }
}

LLVMValueRef LLVMBuildRetMultiple(LLVMBuilderRef builder, 
    LLVMValueRef *values, unsigned n_values)
{
    assert(values);

    std::vector<llvm::Value *> values_vec;
    unwrap_vec(values, n_values, values_vec);

    llvm::IRBuilder<> *builderp = llvm::unwrap(builder);
    assert(builderp);

    return llvm::wrap(builderp->CreateAggregateRet(&values_vec[0], values_vec.size()));
}

LLVMValueRef LLVMBuildGetResult(LLVMBuilderRef builder, 
    LLVMValueRef value, unsigned index, const char *name)
{
    assert(name);

    llvm::IRBuilder<> *builderp = llvm::unwrap(builder);
    assert(builderp);

    return llvm::wrap(builderp->CreateExtractValue(llvm::unwrap(value), index, name));
}

unsigned LLVMValueGetID(LLVMValueRef value)
{
    llvm::Value *valuep = llvm::unwrap(value);
    assert(valuep);

    return valuep->getValueID();
}


unsigned LLVMValueGetNumUses(LLVMValueRef value)
{
    llvm::Value *valuep = llvm::unwrap(value);
    assert(valuep);

    return valuep->getNumUses();
}


unsigned LLVMValueGetUses(LLVMValueRef value, LLVMValueRef **refs)
{
    llvm::Value *valuep = llvm::unwrap(value);
    assert(valuep);

    unsigned n = valuep->getNumUses();
    if (n == 0)
        return 0;

    assert(refs);
    LLVMValueRef *out = (LLVMValueRef *)malloc(sizeof(LLVMValueRef) * n);
    if (!out)
        return 0;
    *refs = out;

    memset(out, 0, sizeof(LLVMValueRef) * n);
    llvm::Value::use_iterator it = valuep->use_begin();
    while (it != valuep->use_end()) {
        *out++ = llvm::wrap(*it);
        ++it;
    }

    return n;
}

unsigned LLVMValueIsUsedInBasicBlock(LLVMValueRef value, LLVMBasicBlockRef bb)
{
    llvm::Value *valuep = llvm::unwrap(value);
    assert(valuep);
    llvm::BasicBlock *bbp = llvm::unwrap(bb);
    assert(bbp);
    return valuep->isUsedInBasicBlock(bbp);
}

void LLVMDisposeValueRefArray(LLVMValueRef *refs)
{
    assert(refs);
    free(refs);
}

unsigned LLVMUserGetNumOperands(LLVMValueRef user)
{
    llvm::User *userp = llvm::unwrap<llvm::User>(user);
    assert(userp);
    return userp->getNumOperands();
}

LLVMValueRef LLVMUserGetOperand(LLVMValueRef user, unsigned idx)
{
    llvm::User *userp = llvm::unwrap<llvm::User>(user);
    assert(userp);
    llvm::Value *operand = userp->getOperand(idx);
    return llvm::wrap(operand);
}

unsigned LLVMGetDoesNotThrow(LLVMValueRef fn)
{
    llvm::Function *fnp = llvm::unwrap<llvm::Function>(fn);
    assert(fnp);

    return fnp->doesNotThrow();
}

void LLVMSetDoesNotThrow(LLVMValueRef fn)
{
    llvm::Function *fnp = llvm::unwrap<llvm::Function>(fn);
    assert(fnp);

    return fnp->setDoesNotThrow();
}

LLVMValueRef LLVMGetIntrinsic(LLVMModuleRef module, int id,
    LLVMTypeRef *types, unsigned n_types)
{
    assert(types);

#if HS_LLVM_VERSION >= 300
    std::vector<llvm::Type*> types_vec;
    unwrap_vec(types, n_types, types_vec);
#else
    std::vector<const llvm::Type*> types_vec;
    unwrap_cvec(types, n_types, types_vec);
#endif

    llvm::Module *modulep = llvm::unwrap(module);
    assert(modulep);

#if HS_LLVM_VERSION >= 300
    llvm::Function *intfunc = llvm::Intrinsic::getDeclaration(modulep, 
        llvm::Intrinsic::ID(id), types_vec);
#else
    llvm::Function *intfunc = llvm::Intrinsic::getDeclaration(modulep, 
        llvm::Intrinsic::ID(id), &types_vec[0], types_vec.size());
#endif
    return wrap(intfunc);
}

LLVMModuleRef LLVMGetModuleFromAssembly(const char *asmtext, unsigned txtlen,
    char **out)
{
    assert(asmtext);
    assert(out);

    llvm::Module *modulep;
    llvm::SMDiagnostic error;
    if (!(modulep = llvm::ParseAssemblyString(asmtext, NULL, error,
                                              llvm::getGlobalContext()))) {
        std::string s;
        llvm::raw_string_ostream buf(s);
        error.print("llvm-py", buf);
        *out = strdup(buf.str().c_str());
        return NULL;
    }

    return wrap(modulep);
}

LLVMModuleRef LLVMGetModuleFromBitcode(const char *bitcode, unsigned bclen,
    char **out)
{
    assert(bitcode);
    assert(out);

    llvm::StringRef as_str(bitcode, bclen);

    llvm::MemoryBuffer *mbp;
    if (!(mbp = llvm::MemoryBuffer::getMemBufferCopy(as_str)))
        return NULL;

    std::string msg;
    llvm::Module *modulep;
    if (!(modulep = llvm::ParseBitcodeFile(mbp, llvm::getGlobalContext(),
                                           &msg)))
        *out = strdup(msg.c_str());

    delete mbp;
    return wrap(modulep);
}

#if HS_LLVM_VERSION < 302
unsigned LLVMLinkModules(LLVMModuleRef dest, LLVMModuleRef src, unsigned mode,
			 char **out)
{
    llvm::Module *sourcep = llvm::unwrap(src);
    assert(sourcep);
    llvm::Module *destinationp = llvm::unwrap(dest);
    assert(destinationp);

    std::string msg;
    bool err;

#if HS_LLVM_VERSION >= 300    
    err = llvm::Linker::LinkModules(destinationp, sourcep, mode, &msg);
#else
    err = llvm::Linker::LinkModules(destinationp, sourcep, &msg);
#endif

    if (err) {
        *out = strdup(msg.c_str());
        return 0;
    }

    return 1;
}
#endif

unsigned char *LLVMGetBitcodeFromModule(LLVMModuleRef module, unsigned *lenp)
{
    assert(lenp);

    llvm::Module *modulep = llvm::unwrap(module);
    assert(modulep);

    /* get bc into a string */
    std::string s;
    llvm::raw_string_ostream buf(s);
    llvm::WriteBitcodeToFile(modulep, buf);
    const std::string& bc = buf.str();

    /* and then into a malloc()-ed block */
    size_t bclen = bc.size();
    unsigned char *bytes = (unsigned char *)malloc(bclen);
    if (!bytes)
        return NULL;
    memcpy(bytes, bc.data(), bclen);

    /* return */
    *lenp = bclen;
    return bytes;
}

/* Return 0 on failure (with errmsg filled in), 1 on success. */
unsigned LLVMLoadLibraryPermanently(const char* filename, char **errmsg)
{
    assert(filename);
    assert(errmsg);

    /* Note: the LLVM API returns true on failure. Don't ask why. */
    std::string msg;
    if (llvm::sys::DynamicLibrary::LoadLibraryPermanently(filename, &msg)) {
        *errmsg = strdup(msg.c_str());
        return 0;
    }

    return 1;
}

void *LLVMGetPointerToFunction(LLVMExecutionEngineRef ee, LLVMValueRef fn)
{
    llvm::ExecutionEngine *eep = llvm::unwrap(ee);
    assert(eep);

    llvm::Function *fnp = llvm::unwrap<llvm::Function>(fn);
    assert(fnp);

    return eep->getPointerToFunction(fnp);
}

int LLVMInlineFunction(LLVMValueRef call)
{
    llvm::Value *callp = llvm::unwrap(call);
    assert(callp);

    llvm::CallSite cs = llvm::CallSite(callp);

    llvm::InlineFunctionInfo unused;
    return llvm::InlineFunction(cs, unused);
}

#if HS_LLVM_VERSION >= 300
// Emits an object file based on the host system's machine specification.
// The object is emitted to the filename given as an argument.
bool LLVMAddEmitObjectPass (LLVMModuleRef modRef, const char* filename)
{
  llvm::InitializeAllTargetInfos ();
  llvm::InitializeAllTargets ();
  llvm::InitializeAllTargetMCs ();
  llvm::InitializeNativeTarget ();
  llvm::InitializeAllAsmPrinters ();

  // will be true post 3.0 I think
  std::string triple = llvm::sys::getDefaultTargetTriple ();

  // std::string triple = llvm::sys::getHostTriple ();
  std::string err;
  const llvm::Target* Target = llvm::TargetRegistry::lookupTarget (triple, err);

  std::string cpu = llvm::sys::getHostCPUName ();
  std::string features = "";

  // llvm::StringMap <bool> featureMap (10);
  
  // // this returns false at the moment, but it appears to not make a huge difference
  // // as the next iteration just doesn't do anything.
  // llvm::sys::getHostCPUFeatures (featureMap);

  // for (  llvm::StringMap <bool>::const_iterator it = featureMap.begin ();
  //        it != featureMap.end ();
  //        ++it) {
  //   if (it->second) {
  //     features += it->first.str() + " ";
  //   }
  // }


  llvm::TargetMachine *machine = 
    Target->createTargetMachine (triple, cpu, features, llvm::TargetOptions());


  llvm::PassManager pass_manager;

  pass_manager.add(new llvm::DataLayout (*machine->getDataLayout()));

  std::string outfile_err;
  llvm::raw_fd_ostream raw_out (filename, outfile_err);
  llvm::formatted_raw_ostream out (raw_out);

  if (machine->addPassesToEmitFile (pass_manager, out,
                                    llvm::TargetMachine::CGFT_ObjectFile,
                                    false))
    return false;

  
  llvm::Module *mod = llvm::unwrap (modRef);
  pass_manager.run (*mod);

  return true;
}
#endif

/* Passes */

/* we support only internalize(true) */
llvm::ModulePass *createInternalize2Pass() {
  return llvm::createInternalizePass(); 
}

/* All passes are listed in passes-inl.h
 * The list is shared between extra.cpp and extra.h.
 *
 * In this file the declare_or_define_pass macro is used to expand
 * the passes into function definitions.
 */
#define declare_or_define_pass(P)                        \
void LLVMAdd ## P ## Pass (LLVMPassManagerRef passmgr) { \
    using namespace llvm;                                \
    llvm::PassManagerBase *pmp = llvm::unwrap(passmgr);  \
    assert(pmp);                                         \
    pmp->add( create ## P ## Pass ());                   \
}
#include "passes-inl.h"
#undef declare_or_define_pass


#if HS_LLVM_VERSION < 302
LLVMBool LLVMPrintModuleToFile(LLVMModuleRef M, const char *Filename,
                               char **ErrorMessage) {
  std::string error;
  llvm::raw_fd_ostream dest(Filename, error);
  if (!error.empty()) {
    *ErrorMessage = strdup(error.c_str());
    return true;
  }

  llvm::unwrap(M)->print(dest, NULL);

  if (!error.empty()) {
    *ErrorMessage = strdup(error.c_str());
    return true;
  }
  dest.flush();
  return false;
}
#endif

#if HS_LLVM_VERSION < 303
LLVMMemoryBufferRef LLVMCreateMemoryBufferWithMemoryRange(
  const char *InputData,
  size_t InputDataLength,
  const char *BufferName,
  LLVMBool RequiresNullTerminator) {

  return wrap(llvm::MemoryBuffer::getMemBuffer(
                llvm::StringRef(InputData, InputDataLength),
                llvm::StringRef(BufferName),
                RequiresNullTerminator));
}

LLVMMemoryBufferRef LLVMCreateMemoryBufferWithMemoryRangeCopy(
  const char *InputData,
  size_t InputDataLength,
  const char *BufferName) {

  return wrap(llvm::MemoryBuffer::getMemBufferCopy(
                llvm::StringRef(InputData, InputDataLength),
                llvm::StringRef(BufferName)));
}
#endif
