{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module LLVM.Core.CodeGenMonad(
    -- * Module code generation
    CodeGenModule, runCodeGenModule, genMSym, getModule,
    -- * Function code generation
    CodeGenFunction, runCodeGenFunction, genFSym, getFunction, getBuilder, getFunctionModule, getExterns, putExterns,
    -- * Reexport
    liftIO
    ) where
import Data.Typeable
import Control.Monad.State

import LLVM.Core.Util(Module, Builder, Function)

--------------------------------------

data CGMState = CGMState {
    cgm_module :: Module,
    cgm_externs :: [(String, Function)],
    cgm_next :: !Int
    }
    deriving (Show, Typeable)
newtype CodeGenModule a = CGM (StateT CGMState IO a)
    deriving (Functor, Monad, MonadState CGMState, MonadIO, Typeable)

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
    let cgm = CGMState { cgm_module = m, cgm_next = 1, cgm_externs = [] }
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
    deriving (Functor, Monad, MonadState (CGFState r), MonadIO, Typeable)

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
