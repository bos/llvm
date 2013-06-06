module ModuleMaker(main) where

import LLVM.Core
import Data.Int

main :: IO ()
main = do
  m <- newNamedModule "test"

  _ <- defineModule m buildMod

  writeBitcodeToFile "ModuleMaker.bc" m

  return ()

buildMod :: CodeGenModule (Function (IO Int32))
buildMod = do 
  _main <- createNamedFunction ExternalLinkage "main" $ do
    addResult <- iadd (valueOf (2::Int32)) (3::Int32)
    ret addResult

  return _main

