{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Core(
    -- * Modules
    Module, newModule, defineModule, destroyModule,
    ModuleProvider, createModuleProviderForExistingModule,
    -- * Instructions
    module LLVM.Core.Instructions,
    -- * Types classification
    module LLVM.Core.Type,
    -- * Extra types
    module LLVM.Core.Data,
    -- * Values
    Value, ConstValue, valueOf, constOf,
    -- * Code generation
    CodeGenFunction, CodeGenModule,
    -- * Functions
    Function,newFunction, defineFunction, createFunction,
    -- * Basic blocks
    BasicBlock, newBasicBlock, defineBasicBlock, createBasicBlock,
    -- * Debugging
    dumpValue
    ) where
import qualified LLVM.Core.FFI as FFI
import LLVM.Core.Util hiding (Function, BasicBlock)
import LLVM.Core.CodeGen
import LLVM.Core.CodeGenMonad(CodeGenFunction, CodeGenModule)
import LLVM.Core.Data
import LLVM.Core.Instructions
import LLVM.Core.Type hiding (IsType)
import LLVM.Core.Type(IsType)

dumpValue :: Value a -> IO ()
dumpValue (Value v) = FFI.dumpValue v
