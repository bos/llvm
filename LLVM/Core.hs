-- |The LLVM (Low Level Virtual Machine) is virtial machine at a machine code level.
-- It supports both stand alone code generation, and also a JIT.
-- The Haskell llvm package is a (relatively) high level interface to the LLVM.
-- The high level interface makes it easy to construct LLVM code.
--
-- LLVM code is organized into modules (type 'Module').
-- Each module contains a number of global variables and functions (type 'Function').
-- Each functions has a number of basic blocks (type 'BasicBlock').
-- Each basic block has a number instructions, where each instruction produces
-- a value (type 'Value').
--
-- Unlike assembly code for a real processor the assembly code for LLVM is
-- on SSA (Static Single Assignment) form.  This means that each instruction generates
-- a new bound variable which may not be assigned again.  Each variable must also
-- only be used in the basic block where it was defined, except for phi instructions.
--
-- The definition of several of the LLVM entities ('Module', 'Function', and 'BasicBlock')
-- follow the same pattern.  First the entity has to be created using @newX@ (where @X@
-- is one of @Module@, @Function@, or @BasicBlock@), then at some later point it has to
-- given its definition using @defineX@.  The reason for splitting the creation and
-- definition is that you often need to be able to refer to an entity before giving
-- it's body, e.g., in two mutually recursive functions.
-- The the @newX@ and @defineX@ function can also be done at the same time by using
-- @createX@.  Furthermore, an explicit name can be given to an entity by the
-- @newNamedX@ function; the @newX@ function just generates a fresh name.
module LLVM.Core(
    -- * Modules
    Module, newModule, newNamedModule, defineModule, destroyModule, createModule,
    ModuleProvider, createModuleProviderForExistingModule,
    PassManager, createPassManager, createFunctionPassManager,
    -- * Instructions
    module LLVM.Core.Instructions,
    -- * Types classification
    module LLVM.Core.Type,
    -- * Extra types
    module LLVM.Core.Data,
    -- * Values and constants
    Value, ConstValue, valueOf, constOf, value,
    constString, constStringNul,
    -- * Code generation
    CodeGenFunction, CodeGenModule,
    -- * Functions
    Function, newFunction, newNamedFunction, defineFunction, createFunction,
    TFunction,
    -- * Global variable creation
    Global, newGlobal, newNamedGlobal, defineGlobal, createGlobal,
    TGlobal,
    -- * Globals
    Linkage(..),
    -- * Basic blocks
    BasicBlock, newBasicBlock, newNamedBasicBlock, defineBasicBlock, createBasicBlock,
    -- * Debugging
    dumpValue,
    -- * Transformations
    addCFGSimplificationPass, addConstantPropagationPass, addDemoteMemoryToRegisterPass,
    addGVNPass, addInstructionCombiningPass, addPromoteMemoryToRegisterPass, addReassociatePass
    ) where
import qualified LLVM.FFI.Core as FFI
import LLVM.Core.Util hiding (Function, BasicBlock, createModule, constString, constStringNul)
import LLVM.Core.CodeGen
import LLVM.Core.CodeGenMonad(CodeGenFunction, CodeGenModule)
import LLVM.Core.Data
import LLVM.Core.Instructions
import LLVM.Core.Type hiding (IsType)
import LLVM.Core.Type(IsType)

-- |Print a value.
dumpValue :: Value a -> IO ()
dumpValue (Value v) = FFI.dumpValue v

{-
dumpType :: forall a . (IsType a) => Value a -> IO ()
dumpType _ = FFI.dumpValue (typeRef (undefined :: a))
-}

-- Enforce free is only called on malloc memory.  (Enforce only one free?)
-- Enforce phi nodes a accessor of variables outside the bb
-- Enforce bb terminator
-- Enforce phi first
