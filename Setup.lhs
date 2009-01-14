#!/usr/bin/env runhaskell
> {-# LANGUAGE PatternGuards #-}
> import System.Environment
> import System.Info
> import Control.Monad
> import Data.List
> import Distribution.Simple
> import Distribution.Simple.Setup
> 
> main = do
>     let hooks = if os == "mingw32" then autoconfUserHooks{ postConf = generateBuildInfo }
>                 else autoconfUserHooks
>     defaultMainWithHooks hooks
> 
> -- On Windows we can't count on the configure script, so generate the
> -- llvm.buildinfo from a template.
> generateBuildInfo _ conf _ _ = do
>     let args = configConfigureArgs conf
>     let pref = "--with-llvm-prefix="
>     let path = case [ p | arg <- args, Just p <- [stripPrefix pref arg] ] of
>                [p] -> p
>                _ -> error $ "Use '--configure-option " ++ pref ++ "PATH' to give LLVM installation path"
>     info <- readFile "llvm.buildinfo.windows.in"
>     writeFile "llvm.buildinfo" $ subst "@llvm_path@" path info
> 
> subst from to [] = []
> subst from to xs | Just r <- stripPrefix from xs = to ++ subst from to r
> subst from to (x:xs) = x : subst from to xs
