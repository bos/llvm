{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, DeriveDataTypeable #-}
module LLVM.Core.Util(
    -- * Module handling
    Module(..), withModule, createModule, destroyModule, writeBitcodeToFile, readBitcodeFromFile,
    getModuleValues, getFunctions, getGlobalVariables, valueHasType,
    -- * Module provider handling
    ModuleProvider(..), withModuleProvider, createModuleProviderForExistingModule,
    -- * Pass manager handling
    PassManager(..), withPassManager, createPassManager, createFunctionPassManager,
    runFunctionPassManager, initializeFunctionPassManager, finalizeFunctionPassManager,
    -- * Instruction builder
    Builder(..), withBuilder, createBuilder, positionAtEnd, getInsertBlock,
    -- * Basic blocks
    BasicBlock,
    appendBasicBlock, getBasicBlocks,
    -- * Functions
    Function,
    addFunction, getParam, getParams,
    -- * Structs
    structType,
    -- * Globals
    addGlobal,
    constString, constStringNul, constVector, constArray, constStruct,
    -- * Instructions
    makeCall, makeInvoke,
    makeCallWithCc, makeInvokeWithCc,
    withValue, getInstructions, getOperands,
    -- * Uses and Users
    hasUsers, getUsers, getUses, getUser, isChildOf, getDep,
    -- * Misc
    CString, withArrayLen,
    withEmptyCString,
    functionType, buildEmptyPhi, addPhiIns,
    showTypeOf, getValueNameU, getObjList, annotateValueList, isConstant,
    -- * Transformation passes
    addCFGSimplificationPass, addConstantPropagationPass, addDemoteMemoryToRegisterPass,
    addGVNPass, addInstructionCombiningPass, addPromoteMemoryToRegisterPass, addReassociatePass,
    addTargetData
    ) where
import Data.Typeable
import Data.List(intercalate)
import Control.Monad(liftM, filterM, when)
import Foreign.C.String (withCString, withCStringLen, CString, peekCString)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, newForeignPtr_, withForeignPtr)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Array (withArrayLen, withArray, allocaArray, peekArray)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable(..))
import System.IO.Unsafe (unsafePerformIO)

import qualified LLVM.FFI.Core as FFI
import qualified LLVM.FFI.Target as FFI
import qualified LLVM.FFI.BitWriter as FFI
import qualified LLVM.FFI.BitReader as FFI
import qualified LLVM.FFI.Transforms.Scalar as FFI

type Type = FFI.TypeRef

-- unsafePerformIO just to wrap the non-effecting withArrayLen call
functionType :: Bool -> Type -> [Type] -> Type
functionType varargs retType paramTypes = unsafePerformIO $
    withArrayLen paramTypes $ \ len ptr ->
        return $ FFI.functionType retType ptr (fromIntegral len) varargs

-- unsafePerformIO just to wrap the non-effecting withArrayLen call
structType :: [Type] -> Bool -> Type
structType types packed = unsafePerformIO $
    withArrayLen types $ \ len ptr ->
        return $ FFI.structType ptr (fromIntegral len) packed

--------------------------------------
-- Handle modules

-- Don't use a finalizer for the module, but instead provide an
-- explicit destructor.  This is because handing a module to
-- a module provider changes ownership of the module to the provider,
-- and we don't want to free it by mistake.

-- | Type of top level modules.
newtype Module = Module {
      fromModule :: FFI.ModuleRef
    }
    deriving (Show, Typeable)

withModule :: Module -> (FFI.ModuleRef -> IO a) -> IO a
withModule modul f = f (fromModule modul)

createModule :: String -> IO Module
createModule name =
    withCString name $ \ namePtr -> do
      liftM Module $ FFI.moduleCreateWithName namePtr

-- | Free all storage related to a module.  *Note*, this is a dangerous call, since referring
-- to the module after this call is an error.  The reason for the explicit call to free
-- the module instead of an automatic lifetime management is that modules have a
-- somewhat complicated ownership.  Handing a module to a module provider changes
-- the ownership of the module, and the module provider will free the module when necessary.
destroyModule :: Module -> IO ()
destroyModule = FFI.disposeModule . fromModule

-- |Write a module to a file.
writeBitcodeToFile :: String -> Module -> IO ()
writeBitcodeToFile name mdl =
    withCString name $ \ namePtr ->
      withModule mdl $ \ mdlPtr -> do
        rc <- FFI.writeBitcodeToFile mdlPtr namePtr
        when (rc /= False) $
          ioError $ userError $ "writeBitcodeToFile: return code " ++ show rc
        return ()

-- |Read a module from a file.
readBitcodeFromFile :: String -> IO Module
readBitcodeFromFile name =
    withCString name $ \ namePtr ->
      alloca $ \ bufPtr ->
      alloca $ \ modPtr ->
      alloca $ \ errStr -> do
        rrc <- FFI.createMemoryBufferWithContentsOfFile namePtr bufPtr errStr
        if rrc /= False then do
            msg <- peek errStr >>= peekCString
            ioError $ userError $ "readBitcodeFromFile: read return code " ++ show rrc ++ ", " ++ msg
         else do
            buf <- peek bufPtr
            prc <- FFI.parseBitcode buf modPtr errStr
	    if prc /= False then do
                msg <- peek errStr >>= peekCString
                ioError $ userError $ "readBitcodeFromFile: parse return code " ++ show prc ++ ", " ++ msg
             else do
                ptr <- peek modPtr
                return $ Module ptr
{-
                liftM Module $ newForeignPtr FFI.ptrDisposeModule ptr
-}

getModuleValues :: Module -> IO [(String, Value)]
getModuleValues mdl = do
  fs <- getFunctions mdl
  gs <- getGlobalVariables mdl
  return (fs ++ gs)

getFunctions :: Module -> IO [(String, Value)]
getFunctions mdl = getObjList withModule FFI.getFirstFunction FFI.getNextFunction mdl >>= filterM isIntrinsic >>= annotateValueList

getGlobalVariables :: Module -> IO [(String, Value)]
getGlobalVariables mdl = getObjList withModule FFI.getFirstGlobal FFI.getNextGlobal mdl >>= annotateValueList

-- This is safe because we just ask for the type of a value.
valueHasType :: Value -> Type -> Bool
valueHasType v t = unsafePerformIO $ do
    vt <- FFI.typeOf v
    return $ vt == t  -- LLVM uses hash consing for types, so pointer equality works.

showTypeOf :: Value -> IO String
showTypeOf v = FFI.typeOf v >>= showType'

showType' :: Type -> IO String
showType' p = do
    pk <- FFI.getTypeKind p
    case pk of
        FFI.VoidTypeKind -> return "()"
	FFI.FloatTypeKind -> return "Float"
	FFI.DoubleTypeKind -> return "Double"
	FFI.X86_FP80TypeKind -> return "X86_FP80"
	FFI.FP128TypeKind -> return "FP128"
	FFI.PPC_FP128TypeKind -> return "PPC_FP128"
	FFI.LabelTypeKind -> return "Label"
	FFI.IntegerTypeKind -> do w <- FFI.getIntTypeWidth p; return $ "(IntN " ++ show w ++ ")"
	FFI.FunctionTypeKind -> do
            r <- FFI.getReturnType p
	    c <- FFI.countParamTypes p
	    let n = fromIntegral c
	    as <- allocaArray n $ \ args -> do
		     FFI.getParamTypes p args
		     peekArray n args
	    ts <- mapM showType' (as ++ [r])
	    return $ "(" ++ intercalate " -> " ts ++ ")"
	FFI.StructTypeKind -> return "(Struct ...)"
	FFI.ArrayTypeKind -> do n <- FFI.getArrayLength p; t <- FFI.getElementType p >>= showType'; return $ "(Array " ++ show n ++ " " ++ t ++ ")"
	FFI.PointerTypeKind -> do t <- FFI.getElementType p >>= showType'; return $ "(Ptr " ++ t ++ ")"
	FFI.OpaqueTypeKind -> return "Opaque"
	FFI.VectorTypeKind -> do n <- FFI.getVectorSize p; t <- FFI.getElementType p >>= showType'; return $ "(Vector " ++ show n ++ " " ++ t ++ ")"

--------------------------------------
-- Handle module providers

-- | A module provider is used by the code generator to get access to a module.
newtype ModuleProvider = ModuleProvider {
      fromModuleProvider :: ForeignPtr FFI.ModuleProvider
    }
    deriving (Show, Typeable)

withModuleProvider :: ModuleProvider -> (FFI.ModuleProviderRef -> IO a)
                   -> IO a
withModuleProvider = withForeignPtr . fromModuleProvider

-- | Turn a module into a module provider.
createModuleProviderForExistingModule :: Module -> IO ModuleProvider
createModuleProviderForExistingModule modul =
    withModule modul $ \modulPtr -> do
        ptr <- FFI.createModuleProviderForExistingModule modulPtr
        -- MPs given to the EE get taken over, so we should not GC them.
        liftM ModuleProvider $ newForeignPtr_ {-FFI.ptrDisposeModuleProvider-} ptr


--------------------------------------
-- Handle instruction builders

newtype Builder = Builder {
      fromBuilder :: ForeignPtr FFI.Builder
    }
    deriving (Show, Typeable)

withBuilder :: Builder -> (FFI.BuilderRef -> IO a) -> IO a
withBuilder = withForeignPtr . fromBuilder

createBuilder :: IO Builder
createBuilder = do
    ptr <- FFI.createBuilder
    liftM Builder $ newForeignPtr FFI.ptrDisposeBuilder ptr

positionAtEnd :: Builder -> FFI.BasicBlockRef -> IO ()
positionAtEnd bld bblk =
    withBuilder bld $ \ bldPtr ->
      FFI.positionAtEnd bldPtr bblk

getInsertBlock :: Builder -> IO FFI.BasicBlockRef
getInsertBlock bld =
    withBuilder bld $ \ bldPtr ->
      FFI.getInsertBlock bldPtr

--------------------------------------

type BasicBlock = FFI.BasicBlockRef

appendBasicBlock :: Function -> String -> IO BasicBlock
appendBasicBlock func name =
    withCString name $ \ namePtr ->
      FFI.appendBasicBlock func namePtr

getBasicBlocks :: Value -> IO [(String, Value)]
getBasicBlocks v = getObjList withValue FFI.getFirstBasicBlock FFI.getNextBasicBlock v >>= annotateValueList

--------------------------------------

type Function = FFI.ValueRef

addFunction :: Module -> FFI.Linkage -> String -> Type -> IO Function
addFunction modul linkage name typ =
    withModule modul $ \ modulPtr ->
      withCString name $ \ namePtr -> do
        f <- FFI.addFunction modulPtr namePtr typ
        FFI.setLinkage f (FFI.fromLinkage linkage)
        return f

getParam :: Function -> Int -> Value
getParam f = FFI.getParam f . fromIntegral

getParams :: Value -> IO [(String, Value)]
getParams v = getObjList withValue FFI.getFirstParam FFI.getNextParam v >>= annotateValueList

--------------------------------------

addGlobal :: Module -> FFI.Linkage -> String -> Type -> IO Value
addGlobal modul linkage name typ =
    withModule modul $ \ modulPtr ->
      withCString name $ \ namePtr -> do
        v <- FFI.addGlobal modulPtr typ namePtr
        FFI.setLinkage v (FFI.fromLinkage linkage)
        return v

-- unsafePerformIO is safe because it's only used for the withCStringLen conversion
constStringInternal :: Bool -> String -> (Value, Int)
constStringInternal nulTerm s = unsafePerformIO $
    withCStringLen s $ \(sPtr, sLen) ->
      return (FFI.constString sPtr (fromIntegral sLen) (not nulTerm), sLen)

constString :: String -> (Value, Int)
constString = constStringInternal False

constStringNul :: String -> (Value, Int)
constStringNul str =
    let (cstr, n) = constStringInternal True str
    in (cstr, n+1)

--------------------------------------

type Value = FFI.ValueRef

withValue :: Value -> (Value -> IO a) -> IO a
withValue v f = f v

makeCall :: Function -> FFI.BuilderRef -> [Value] -> IO Value
makeCall = makeCallWithCc FFI.C

makeCallWithCc :: FFI.CallingConvention -> Function -> FFI.BuilderRef -> [Value] -> IO Value
makeCallWithCc cc func bldPtr args = do
{-
      print "makeCall"
      FFI.dumpValue func
      mapM_ FFI.dumpValue args
      print "----------------------"
-}
      withArrayLen args $ \ argLen argPtr ->
        withEmptyCString $ \cstr -> do
          i <- FFI.buildCall bldPtr func argPtr
                             (fromIntegral argLen) cstr
          FFI.setInstructionCallConv i (FFI.fromCallingConvention cc)
          return i

makeInvoke :: BasicBlock -> BasicBlock -> Function -> FFI.BuilderRef ->
              [Value] -> IO Value
makeInvoke = makeInvokeWithCc FFI.C

makeInvokeWithCc :: FFI.CallingConvention -> BasicBlock -> BasicBlock -> Function -> FFI.BuilderRef ->
              [Value] -> IO Value
makeInvokeWithCc cc norm expt func bldPtr args =
      withArrayLen args $ \ argLen argPtr ->
        withEmptyCString $ \cstr -> do
          i <- FFI.buildInvoke bldPtr func argPtr (fromIntegral argLen) norm expt cstr
          FFI.setInstructionCallConv i (FFI.fromCallingConvention cc)
          return i

getInstructions :: Value -> IO [(String, Value)]
getInstructions bb = getObjList withValue FFI.getFirstInstruction FFI.getNextInstruction bb >>= annotateValueList

getOperands :: Value -> IO [(String, Value)]
getOperands ii = geto ii >>= annotateValueList
    where geto i = do
            num <- FFI.getNumOperands i
            let oloop instr number total = if number >= total then return [] else do
                    o <- FFI.getOperand instr number
                    os <- oloop instr (number + 1) total
                    return (o : os)
            oloop i 0 num

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

-- | Manage compile passes.
newtype PassManager = PassManager {
      fromPassManager :: ForeignPtr FFI.PassManager
    }
    deriving (Show, Typeable)

withPassManager :: PassManager -> (FFI.PassManagerRef -> IO a)
                   -> IO a
withPassManager = withForeignPtr . fromPassManager

-- | Create a pass manager.
createPassManager :: IO PassManager
createPassManager = do
    ptr <- FFI.createPassManager
    liftM PassManager $ newForeignPtr FFI.ptrDisposePassManager ptr

-- | Create a pass manager for a module.
createFunctionPassManager :: ModuleProvider -> IO PassManager
createFunctionPassManager modul =
    withModuleProvider modul $ \modulPtr -> do
        ptr <- FFI.createFunctionPassManager modulPtr
        liftM PassManager $ newForeignPtr FFI.ptrDisposePassManager ptr

-- | Add a control flow graph simplification pass to the manager.
addCFGSimplificationPass :: PassManager -> IO ()
addCFGSimplificationPass pm = withPassManager pm FFI.addCFGSimplificationPass

-- | Add a constant propagation pass to the manager.
addConstantPropagationPass :: PassManager -> IO ()
addConstantPropagationPass pm = withPassManager pm FFI.addConstantPropagationPass

addDemoteMemoryToRegisterPass :: PassManager -> IO ()
addDemoteMemoryToRegisterPass pm = withPassManager pm FFI.addDemoteMemoryToRegisterPass

-- | Add a global value numbering pass to the manager.
addGVNPass :: PassManager -> IO ()
addGVNPass pm = withPassManager pm FFI.addGVNPass

addInstructionCombiningPass :: PassManager -> IO ()
addInstructionCombiningPass pm = withPassManager pm FFI.addInstructionCombiningPass

addPromoteMemoryToRegisterPass :: PassManager -> IO ()
addPromoteMemoryToRegisterPass pm = withPassManager pm FFI.addPromoteMemoryToRegisterPass

addReassociatePass :: PassManager -> IO ()
addReassociatePass pm = withPassManager pm FFI.addReassociatePass

addTargetData :: FFI.TargetDataRef -> PassManager -> IO ()
addTargetData td pm = withPassManager pm $ FFI.addTargetData td

runFunctionPassManager :: PassManager -> Function -> IO Bool
runFunctionPassManager pm fcn = withPassManager pm $ \ pmref -> FFI.runFunctionPassManager pmref fcn

initializeFunctionPassManager :: PassManager -> IO Bool
initializeFunctionPassManager pm = withPassManager pm FFI.initializeFunctionPassManager

finalizeFunctionPassManager :: PassManager -> IO Bool
finalizeFunctionPassManager pm = withPassManager pm FFI.finalizeFunctionPassManager

--------------------------------------

-- The unsafePerformIO is just for the non-effecting withArrayLen
constVector :: Int -> [Value] -> Value
constVector n xs = unsafePerformIO $ do
    let xs' = take n (cycle xs)
    withArrayLen xs' $ \ len ptr ->
        return $ FFI.constVector ptr (fromIntegral len)

-- The unsafePerformIO is just for the non-effecting withArrayLen
constArray :: Type -> Int -> [Value] -> Value
constArray t n xs = unsafePerformIO $ do
    let xs' = take n (cycle xs)
    withArrayLen xs' $ \ len ptr ->
        return $ FFI.constArray t ptr (fromIntegral len)

-- The unsafePerformIO is just for the non-effecting withArrayLen
constStruct :: [Value] -> Bool -> Value
constStruct xs packed = unsafePerformIO $ do
    withArrayLen xs $ \ len ptr ->
        return $ FFI.constStruct ptr (fromIntegral len) packed

--------------------------------------

getValueNameU :: Value -> IO String
getValueNameU a = do
    -- sometimes void values need explicit names too
    cs <- FFI.getValueName a
    str <- peekCString cs
    if str == "" then return (show a) else return str

getObjList :: (t1 -> (t2 -> IO [Ptr a]) -> t) -> (t2 -> IO (Ptr a))
           -> (Ptr a -> IO (Ptr a)) -> t1 -> t
getObjList withF firstF nextF obj = do
    withF obj $ \ objPtr -> do
      ofst <- firstF objPtr 
      let oloop p = if p == nullPtr then return [] else do
              n <- nextF p
              ps <- oloop n
              return (p : ps)
      oloop ofst

annotateValueList :: [Value] -> IO [(String, Value)]
annotateValueList vs = do
  names <- mapM getValueNameU vs
  return $ zip names vs

isConstant :: Value -> IO Bool
isConstant = FFI.isConstant

isIntrinsic :: Value -> IO Bool
isIntrinsic v = do
  if FFI.getIntrinsicID v == 0 then return True else return False

--------------------------------------

type Use = FFI.UseRef

hasUsers :: Value -> IO Bool
hasUsers v = do
  nU <- FFI.getNumUses v
  if nU == 0 then return False else return True

getUses :: Value -> IO [Use]
getUses = getObjList withValue FFI.getFirstUse FFI.getNextUse

getUsers :: [Use] -> IO [(String, Value)]
getUsers us = mapM FFI.getUser us >>= annotateValueList

getUser :: Use -> IO Value
getUser = FFI.getUser

isChildOf :: BasicBlock -> Value -> IO Bool
isChildOf bb v = do
  bb2 <- FFI.getInstructionParent v
  if bb == bb2 then return True else return False

getDep :: Use -> IO (String, String)
getDep u = do
  producer <- FFI.getUsedValue u >>= getValueNameU
  consumer <- FFI.getUser u >>= getValueNameU
  return (producer, consumer)
