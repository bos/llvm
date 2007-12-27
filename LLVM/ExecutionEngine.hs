module LLVM.ExecutionEngine
    (
      ExecutionEngine
    , createExecutionEngine
    ) where

import Foreign.ForeignPtr (ForeignPtr)

import qualified LLVM
import qualified LLVM.ExecutionEngine.Base as EE

newtype ExecutionEngine = ExecutionEngine {
      fromEE :: ForeignPtr EE.ExecutionEngine
    }

createExecutionEngine :: LLVM.ModuleProvider -> IO ExecutionEngine
createExecutionEngine prov = undefined
