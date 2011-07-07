-- |The LLVM (Low Level Virtual Machine) is virtual machine at a machine code level.
-- It supports both stand alone code generation and JITing.
-- The Haskell llvm package is a (relatively) high level interface to the LLVM.
-- The high level interface makes it easy to construct LLVM code.
-- There is also an interface to the raw low level LLVM API as exposed by the LLVM C interface.
--
-- LLVM code is organized into modules (type 'Module').
-- Each module contains a number of global variables and functions (type 'Function').
-- Each functions has a number of basic blocks (type 'BasicBlock').
-- Each basic block has a number instructions, where each instruction produces
-- a value (type 'Value').
--
-- Unlike assembly code for a real processor the assembly code for LLVM is
-- in SSA (Static Single Assignment) form.  This means that each instruction generates
-- a new bound variable which may not be assigned again.
-- A consequence of this is that where control flow joins from several execution
-- paths there has to be a phi pseudo instruction if you want different variables
-- to be joined into one.
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
    -- * Initialize
    initializeNativeTarget,
    -- * Modules
    Module, newModule, newNamedModule, defineModule, destroyModule, createModule,
    ModuleProvider, createModuleProviderForExistingModule,
    PassManager, createPassManager, createFunctionPassManager,
    writeBitcodeToFile, readBitcodeFromFile,
    getModuleValues, getFunctions, getGlobalVariables, ModuleValue, castModuleValue,
    -- * Instructions
    module LLVM.Core.Instructions,
    -- * Types classification
    module LLVM.Core.Type,
    -- * Extra types
    module LLVM.Core.Data,
    -- * Values and constants
    Value, ConstValue, valueOf, constOf, value,
    zero, allOnes, undef,
    createString, createStringNul,
    withString, withStringNul,
    --constString, constStringNul,
    constVector, constArray,
    constStruct, constPackedStruct,
    toVector, fromVector, vector,
    -- * Code generation
    CodeGenFunction, CodeGenModule,
    -- * Functions
    Function, newFunction, newNamedFunction, defineFunction, createFunction, createNamedFunction, setFuncCallConv,
    TFunction, liftCodeGenModule, getParams,
    -- * Global variable creation
    Global, newGlobal, newNamedGlobal, defineGlobal, createGlobal, createNamedGlobal,
    externFunction, staticFunction,
    externGlobal, staticGlobal,
    GlobalMappings, getGlobalMappings,
    TGlobal,
    -- * Globals
    Linkage(..),
    -- * Basic blocks
    BasicBlock, newBasicBlock, newNamedBasicBlock, defineBasicBlock, createBasicBlock, createNamedBasicBlock, getCurrentBasicBlock,
    getBasicBlocks,              
    fromLabel, toLabel,
    getInstructions, getOperands, hasUsers, getUsers, getUses, getUser, isChildOf, getDep,
    -- * Misc
    addAttributes, Attribute(..),
    castVarArgs,
    -- * Debugging
    dumpValue, dumpType, getValueName, annotateValueList
    ) where
import qualified LLVM.FFI.Core as FFI
import LLVM.Core.Util hiding (Function, BasicBlock, createModule, constString, constStringNul, constVector, constArray, constStruct, getModuleValues, valueHasType)
import LLVM.Core.CodeGen
import LLVM.Core.CodeGenMonad(CodeGenFunction, CodeGenModule, liftCodeGenModule, GlobalMappings, getGlobalMappings)
import LLVM.Core.Data
import LLVM.Core.Instructions
import LLVM.Core.Type
import LLVM.Core.Vector
import LLVM.Target.Native

-- |Print a value.
dumpValue :: Value a -> IO ()
dumpValue (Value v) = FFI.dumpValue v

-- |Print a type.
dumpType :: Value a -> IO ()
dumpType (Value v) = showTypeOf v >>= putStrLn

-- |Get the name of a 'Value'.
getValueName :: Value a -> IO String
getValueName (Value a) = getValueNameU a

-- |Convert a varargs function to a regular function.
castVarArgs :: (CastVarArgs a b) => Function a -> Function b
castVarArgs (Value a) = Value a

-- TODO for types:
-- Enforce free is only called on malloc memory.  (Enforce only one free?)
-- Enforce phi nodes a accessor of variables outside the bb
-- Enforce bb terminator
-- Enforce phi first
--
-- TODO:
-- Add Struct, PackedStruct types
-- Get alignment from code gen
