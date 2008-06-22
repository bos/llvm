{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Core.Util(
    -- * Module handling
    Module(..), withModule, createModule, destroyModule,
    -- * Module provider handling
    ModuleProvider(..), withModuleProvider, createModuleProviderForExistingModule,
    -- * Pass manager handling
    PassManager(..), withPassManager, createPassManager, createFunctionPassManager,
    -- * Instruction builder
    Builder(..), withBuilder, createBuilder, positionAtEnd,
    -- * Basic blocks
    BasicBlock,
    appendBasicBlock,
    -- * Functions
    Function,
    addFunction, getParam,
    -- * Globals
    addGlobal,
    constString, constStringNul,
    -- * Instructions
    makeCall, makeInvoke,
    -- * Misc
    CString, withArrayLen,
    withEmptyCString,
    functionType, buildEmptyPhi, addPhiIns,
    -- * Transformation passes
    addCFGSimplificationPass, addConstantPropagationPass, addDemoteMemoryToRegisterPass,
    addGVNPass, addInstructionCombiningPass, addPromoteMemoryToRegisterPass, addReassociatePass
    ) where
import Control.Monad(liftM)
import Foreign.C.String (withCString, withCStringLen, CString)
import Foreign.ForeignPtr (ForeignPtr, FinalizerPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Array (withArrayLen, withArray)
import Foreign.Marshal.Utils (fromBool)
import System.IO.Unsafe (unsafePerformIO)

import qualified LLVM.Core.FFI as FFI
import qualified LLVM.Transforms.ScalarFFI as FFI

type Type = FFI.TypeRef

-- unsafePerformIO just to wrap the non-effecting withArrayLen call
functionType :: Bool -> Type -> [Type] -> Type
functionType varargs retType paramTypes = unsafePerformIO $
    withArrayLen paramTypes $ \ len ptr ->
        return $ FFI.functionType retType ptr (fromIntegral len)
	       	 		  (fromBool varargs)

--------------------------------------
-- Handle modules
{-
newtype Module = Module {
      fromModule :: ForeignPtr FFI.Module
    }
--    deriving (Typeable)

withModule :: Module -> (FFI.ModuleRef -> IO a) -> IO a
withModule modul = withForeignPtr (fromModule modul)

createModule :: String -> IO Module
createModule name =
    withCString name $ \namePtr -> do
      ptr <- FFI.moduleCreateWithName namePtr
      final <- h2c_module FFI.disposeModule
      liftM Module $ newForeignPtr final ptr

foreign import ccall "wrapper" h2c_module
    :: (FFI.ModuleRef -> IO ()) -> IO (FinalizerPtr a)
-}

-- Don't use a finalizer for the module, but instead provide an
-- explicit destructor.  This is because handing a module to
-- a module provider changes ownership of the module to the provider,
-- and we don't want to free it by mistake.

-- | Type of top level modules.
newtype Module = Module {
      fromModule :: FFI.ModuleRef
    }

withModule :: Module -> (FFI.ModuleRef -> IO a) -> IO a
withModule modul f = f (fromModule modul)

createModule :: String -> IO Module
createModule name =
    withCString name $ \namePtr -> do
      liftM Module $ FFI.moduleCreateWithName namePtr

destroyModule :: Module -> IO ()
destroyModule = FFI.disposeModule . fromModule

--------------------------------------
-- Handle module providers

-- | A module provider is used by the code generator to get access to a module.
newtype ModuleProvider = ModuleProvider {
      fromModuleProvider :: ForeignPtr FFI.ModuleProvider
    }

withModuleProvider :: ModuleProvider -> (FFI.ModuleProviderRef -> IO a)
                   -> IO a
withModuleProvider prov = withForeignPtr (fromModuleProvider prov)

-- | Turn a module into a module provider.
createModuleProviderForExistingModule :: Module -> IO ModuleProvider
createModuleProviderForExistingModule modul =
    withModule modul $ \modulPtr -> do
        ptr <- FFI.createModuleProviderForExistingModule modulPtr
        final <- h2c_moduleProvider FFI.disposeModuleProvider
        liftM ModuleProvider $ newForeignPtr final ptr

foreign import ccall "wrapper" h2c_moduleProvider
    :: (FFI.ModuleProviderRef -> IO ()) -> IO (FinalizerPtr a)


--------------------------------------
-- Handle instruction builders

newtype Builder = Builder {
      fromBuilder :: ForeignPtr FFI.Builder
    }

withBuilder :: Builder -> (FFI.BuilderRef -> IO a) -> IO a
withBuilder = withForeignPtr . fromBuilder

createBuilder :: IO Builder
createBuilder = do
    final <- h2c_builder FFI.disposeBuilder
    ptr <- FFI.createBuilder
    liftM Builder $ newForeignPtr final ptr

foreign import ccall "wrapper" h2c_builder
    :: (FFI.BuilderRef -> IO ()) -> IO (FinalizerPtr a)

positionAtEnd :: Builder -> FFI.BasicBlockRef -> IO ()
positionAtEnd bld bblk =
    withBuilder bld $ \ bldPtr ->
      FFI.positionAtEnd bldPtr bblk

--------------------------------------

type BasicBlock = FFI.BasicBlockRef

appendBasicBlock :: Function -> String -> IO BasicBlock
appendBasicBlock func name =
    withCString name $ \ namePtr ->
      FFI.appendBasicBlock func namePtr

--------------------------------------

type Function = FFI.ValueRef

addFunction :: Module -> FFI.Linkage -> String -> Type -> IO Function
addFunction modul linkage name typ =
    withModule modul $ \ modulPtr ->
      withCString name $ \ namePtr -> do
        f <- FFI.addFunction modulPtr namePtr typ
        FFI.setLinkage f linkage
        return f

getParam :: Function -> Int -> Value
getParam f n = FFI.getParam f (fromIntegral n)

--------------------------------------

addGlobal :: Module -> FFI.Linkage -> String -> Type -> IO Value
addGlobal modul linkage name typ =
    withModule modul $ \ modulPtr ->
      withCString name $ \ namePtr -> do
        v <- FFI.addGlobal modulPtr typ namePtr
        FFI.setLinkage v linkage
        return v

-- unsafePerformIO is safe because it's only used for the withCStringLen conversion
constStringInternal :: Bool -> String -> Value
constStringInternal nulTerm s = unsafePerformIO $
    withCStringLen s $ \(sPtr, sLen) ->
      return $ FFI.constString sPtr (fromIntegral sLen) (fromBool (not nulTerm))

constString :: String -> Value
constString = constStringInternal False

constStringNul :: String -> Value
constStringNul = constStringInternal True

--------------------------------------

type Value = FFI.ValueRef

makeCall :: Function -> FFI.BuilderRef -> [Value] -> IO Value
makeCall func bldPtr args = do
{-
      print "makeCall"
      FFI.dumpValue func
      mapM_ FFI.dumpValue args
      print "----------------------"
-}
      withArrayLen args $ \ argLen argPtr ->
        withEmptyCString $ 
          FFI.buildCall bldPtr func argPtr
                        (fromIntegral argLen)

makeInvoke :: BasicBlock -> BasicBlock -> Function -> FFI.BuilderRef ->
              [Value] -> IO Value
makeInvoke norm expt func bldPtr args =
      withArrayLen args $ \ argLen argPtr ->
        withEmptyCString $ 
          FFI.buildInvoke bldPtr func argPtr (fromIntegral argLen) norm expt

--------------------------------------

buildEmptyPhi :: FFI.BuilderRef -> Type -> IO Value
buildEmptyPhi bldPtr typ = do
    withEmptyCString $ FFI.buildPhi bldPtr typ

withEmptyCString :: (CString -> IO a) -> IO a
withEmptyCString = withCString "" 

addPhiIns :: Value -> [(Value, BasicBlock)] -> IO ()
addPhiIns inst incoming = do
    let (vals, bblks) = unzip incoming
    withArrayLen vals $ \ count valPtr ->
      withArray bblks $ \ bblkPtr ->
        FFI.addIncoming inst valPtr bblkPtr (fromIntegral count)

--------------------------------------

newtype PassManager = PassManager {
      fromPassManager :: ForeignPtr FFI.PassManager
    }

withPassManager :: PassManager -> (FFI.PassManagerRef -> IO a)
                   -> IO a
withPassManager prov = withForeignPtr (fromPassManager prov)

createPassManager :: IO PassManager
createPassManager = do
    ptr <- FFI.createPassManager
    final <- h2c_passManager FFI.disposePassManager
    liftM PassManager $ newForeignPtr final ptr

createFunctionPassManager :: ModuleProvider -> IO PassManager
createFunctionPassManager modul =
    withModuleProvider modul $ \modulPtr -> do
        ptr <- FFI.createFunctionPassManager modulPtr
        final <- h2c_passManager FFI.disposePassManager
        liftM PassManager $ newForeignPtr final ptr

foreign import ccall "wrapper" h2c_passManager
    :: (FFI.PassManagerRef -> IO ()) -> IO (FinalizerPtr a)

addCFGSimplificationPass :: PassManager -> IO ()
addCFGSimplificationPass pm = withPassManager pm FFI.addCFGSimplificationPass

addConstantPropagationPass :: PassManager -> IO ()
addConstantPropagationPass pm = withPassManager pm FFI.addConstantPropagationPass

addDemoteMemoryToRegisterPass :: PassManager -> IO ()
addDemoteMemoryToRegisterPass pm = withPassManager pm FFI.addDemoteMemoryToRegisterPass

addGVNPass :: PassManager -> IO ()
addGVNPass pm = withPassManager pm FFI.addGVNPass

addInstructionCombiningPass :: PassManager -> IO ()
addInstructionCombiningPass pm = withPassManager pm FFI.addInstructionCombiningPass

addPromoteMemoryToRegisterPass :: PassManager -> IO ()
addPromoteMemoryToRegisterPass pm = withPassManager pm FFI.addPromoteMemoryToRegisterPass

addReassociatePass :: PassManager -> IO ()
addReassociatePass pm = withPassManager pm FFI.addReassociatePass
