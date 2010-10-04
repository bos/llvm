module CallConv where
import Prelude hiding(and, or)
import System.Environment(getArgs)
import Control.Monad(forM_)
import Data.Word

import LLVM.Core
import LLVM.Util.Optimize
import LLVM.ExecutionEngine
import LLVM.FFI.Core (CallingConvention(..))

-- Our module will have these two functions.
data Mod = Mod {
    m1 :: Function (Word32 -> IO Word32),
    m2 :: Function (Word32 -> Word32 -> IO Word32)
    }

main :: IO ()
main = do
    m <- newModule
    fns <- defineModule m buildMod
    --_ <- optimizeModule 3 m
    writeBitcodeToFile "CallConv.bc" m
    return ()

buildMod :: CodeGenModule Mod
buildMod = do
    m2 <- createNamedFunction InternalLinkage "plus" $ \ x y -> do
      r <- add x y
      ret r
    setFuncCallConv m2 GHC
    m1 <- newNamedFunction ExternalLinkage "test"
    defineFunction m1 $ \ arg -> do
      r <- callWithConv GHC m2 arg (valueOf 1)
      ret r
    return $ Mod m1 m2
