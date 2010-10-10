module CallConv where

import LLVM.Core
import LLVM.FFI.Core (CallingConvention(GHC))

import Data.Word (Word32)


-- Our module will have these two functions.
data Mod = Mod {
    m1 :: Function (Word32 -> IO Word32),
    m2 :: Function (Word32 -> Word32 -> IO Word32)
    }

main :: IO ()
main = do
    m <- newModule
    _fns <- defineModule m buildMod
    --_ <- optimizeModule 3 m
    writeBitcodeToFile "CallConv.bc" m
    return ()

buildMod :: CodeGenModule Mod
buildMod = do
    mod2 <- createNamedFunction InternalLinkage "plus" $ \ x y -> do
      r <- add x y
      ret r
    setFuncCallConv mod2 GHC
    mod1 <- newNamedFunction ExternalLinkage "test"
    defineFunction mod1 $ \ arg -> do
      r <- callWithConv GHC mod2 arg (valueOf 1)
      ret r
    return $ Mod mod1 mod2
