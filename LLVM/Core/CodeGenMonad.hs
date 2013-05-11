{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module LLVM.Core.CodeGenMonad(
    -- * Module code generation
    CodeGenModule, runCodeGenModule, genMSym, getModule,
    GlobalMappings(..), addGlobalMapping, getGlobalMappings,
    -- * Function code generation
    CodeGenFunction, runCodeGenFunction, liftCodeGenModule, genFSym, getFunction, getBuilder, getFunctionModule, getExterns, putExterns,
    -- * Reexport
    liftIO
    ) where
import Data.Typeable
import Control.Monad.State
import Control.Applicative (Applicative, )

import Foreign.Ptr (Ptr, )

import LLVM.Core.Util(Module, Builder, Function)

--------------------------------------

data CGMState = CGMState {
    cgm_module :: Module,
    cgm_externs :: [(String, Function)],
    cgm_global_mappings :: [(Function, Ptr ())],
    cgm_next :: !Int
    }
    deriving (Show, Typeable)
newtype CodeGenModule a = CGM (StateT CGMState IO a)
    deriving (Functor, Applicative, Monad, MonadState CGMState, MonadIO, Typeable)

genMSym :: String -> CodeGenModule String
genMSym prefix = do
    s <- get
    let n = cgm_next s
    put (s { cgm_next = n + 1 })
    return $ "_" ++ prefix ++ show n

getModule :: CodeGenModule Module
getModule = gets cgm_module

runCodeGenModule :: Module -> CodeGenModule a -> IO a
runCodeGenModule m (CGM body) = do
    let cgm = CGMState { cgm_module = m, cgm_next = 1, cgm_externs = [], cgm_global_mappings = [] }
    evalStateT body cgm

--------------------------------------

data CGFState r = CGFState {
    cgf_module :: CGMState,
    cgf_builder :: Builder,
    cgf_function :: Function,
    cgf_next :: !Int
    }
    deriving (Show, Typeable)
newtype CodeGenFunction r a = CGF (StateT (CGFState r) IO a)
    deriving (Functor, Applicative, Monad, MonadState (CGFState r), MonadIO, Typeable)

genFSym :: CodeGenFunction a String
genFSym = do
    s <- get
    let n = cgf_next s
    put (s { cgf_next = n + 1 })
    return $ "_L" ++ show n

getFunction :: CodeGenFunction a Function
getFunction = gets cgf_function

getBuilder :: CodeGenFunction a Builder
getBuilder = gets cgf_builder

getFunctionModule :: CodeGenFunction a Module
getFunctionModule = gets (cgm_module . cgf_module)

getExterns :: CodeGenFunction a [(String, Function)]
getExterns = gets (cgm_externs . cgf_module)

putExterns :: [(String, Function)] -> CodeGenFunction a ()
putExterns es = do
    cgf <- get
    let cgm' = (cgf_module cgf) { cgm_externs = es }
    put (cgf { cgf_module = cgm' })

addGlobalMapping ::
    Function -> Ptr () -> CodeGenModule ()
addGlobalMapping value func = modify $ \cgm ->
        cgm { cgm_global_mappings =
                 (value,func) : cgm_global_mappings cgm }

newtype GlobalMappings =
   GlobalMappings [(Function, Ptr ())]

{- |
Get a list created by calls to 'staticFunction'
that must be passed to the execution engine
via 'LLVM.ExecutionEngine.addGlobalMappings'.
-}
getGlobalMappings ::
    CodeGenModule GlobalMappings
getGlobalMappings =
   gets (GlobalMappings . cgm_global_mappings)

runCodeGenFunction :: Builder -> Function -> CodeGenFunction r a -> CodeGenModule a
runCodeGenFunction bld fn (CGF body) = do
    cgm <- get
    let cgf = CGFState { cgf_module = cgm,
                         cgf_builder = bld,
                         cgf_function = fn,
                         cgf_next = 1 }
    (a, cgf') <- liftIO $ runStateT body cgf
    put (cgf_module cgf')
    return a

--------------------------------------

-- | Allows you to define part of a module while in the middle of defining a function.
liftCodeGenModule :: CodeGenModule a -> CodeGenFunction r a
liftCodeGenModule (CGM act) = do
    cgf <- get
    (a, cgm') <- liftIO $ runStateT act (cgf_module cgf)
    put (cgf { cgf_module = cgm' })
    return a
