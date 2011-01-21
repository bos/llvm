module LLVM.Util.File(writeCodeGenModule, optimizeFunction, optimizeFunctionCG) where
import System.Cmd(system)

import LLVM.Core
import LLVM.ExecutionEngine

writeCodeGenModule :: FilePath -> CodeGenModule a -> IO ()
writeCodeGenModule name f = do
    m <- newModule
    _ <- defineModule m f
    writeBitcodeToFile name m

optimize :: FilePath -> IO ()
optimize name = do
    _rc <- system $ "opt -std-compile-opts " ++ name ++ " -f -o " ++ name
    return ()

optimizeFunction :: (IsType t, Translatable t) => CodeGenModule (Function t) -> IO (Function t)
optimizeFunction = fmap snd . optimizeFunction'

optimizeFunction' :: (IsType t, Translatable t) => CodeGenModule (Function t) -> IO (Module, Function t)
optimizeFunction' mdl = do
    m <- newModule
    mf <- defineModule m mdl
    fName <- getValueName mf

    let name = "__tmp__" ++ fName ++ ".bc"
    writeBitcodeToFile name m

    optimize name

    m' <- readBitcodeFromFile name
    funcs <- getModuleValues m'

--    removeFile name

    let Just mf' = castModuleValue =<< lookup fName funcs

    return (m', mf')

optimizeFunctionCG :: (IsType t, Translatable t) => CodeGenModule (Function t) -> IO t
optimizeFunctionCG mdl = do
    (m', mf') <- optimizeFunction' mdl
    rf <- runEngineAccess $ do
        addModule m'
        generateFunction mf'
    return rf
