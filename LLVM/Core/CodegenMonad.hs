{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LLVM.Core.CodeGenMonad(
    -- * Module code generation
    CodeGenModule, runCodeGenModule, genMSym, getModule,
    -- * Function code generation
    CodeGenFunction, runCodeGenFunction, genFSym, getFunction, getBuilder,
    -- * Reexport
    liftIO
    ) where
import Control.Monad.State

import LLVM.Core.Util(Module, Builder, Function)

--------------------------------------

data CGMState = CGMState {
    cgm_module :: Module,
    cgm_next :: !Int
    }
newtype CodeGenModule a = CGM (StateT CGMState IO a)
    deriving (Monad, MonadState CGMState, MonadIO)

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
    let cgm = CGMState { cgm_module = m, cgm_next = 1 }
    evalStateT body cgm

--------------------------------------

data CGFState r = CGFState { 
    cgf_builder :: Builder,
    cgf_function :: Function,
    cgf_next :: !Int
    }
newtype CodeGenFunction r a = CGF (StateT (CGFState r) IO a)
    deriving (Monad, MonadState (CGFState r), MonadIO)

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

runCodeGenFunction :: Builder -> Function -> CodeGenFunction r a -> CodeGenModule a
runCodeGenFunction bld fn (CGF body) = do
    let cgf = CGFState { cgf_builder = bld,
    	      	       	 cgf_function = fn,
			 cgf_next = 1 }
    liftIO $ evalStateT body cgf
