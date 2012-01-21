{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
import System.Directory
import System.Environment
import System.FilePath
import System.Info
import Control.Monad
import Data.List
import Data.Maybe
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.InstalledPackageInfo
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Install
import Distribution.Simple.Register
import Distribution.Simple.Utils
import Distribution.Text ( display )

main = do
    let hooks = autoconfUserHooks { postConf = if os == "mingw32" 
                                               then generateBuildInfo 
                                               else postConf autoconfUserHooks
                                  , instHook = installHookWithExtraGhciLibraries
                                  , regHook  = regHookWithExtraGhciLibraries
                                  }
    defaultMainWithHooks hooks

-- On Windows we can't count on the configure script, so generate the
-- llvm.buildinfo from a template.
generateBuildInfo _ conf _ _ = do
    let args = configConfigureArgs conf
    let pref = "--with-llvm-prefix="
    let path = case [ p | arg <- args, Just p <- [stripPrefix pref arg] ] of
               [p] -> p
               _ -> error $ "Use '--configure-option " ++ pref ++ "PATH' to give LLVM installation path"
    info <- readFile "llvm.buildinfo.windows.in"
    writeFile "llvm.buildinfo" $ subst "@llvm_path@" path info

subst from to [] = []
subst from to xs | Just r <- stripPrefix from xs = to ++ subst from to r
subst from to (x:xs) = x : subst from to xs

{-
To compensate for Cabal's current design,
we need to replicate the default registration hook code here,
to inject a value for extra-ghci-libraries into the package registration info.
(Inspired by 'Gtk2HsSetup.hs'.)
This only works for Cabal 1.10,
thus we added an according constraint to llvm.cabal.

We define an extension field 'x-extra-ghci-libraries' in the .buildinfo file
in order to communicate the version information of the LLVM dynamic library
from the configure script to the registration code.
-}
installHookWithExtraGhciLibraries :: PackageDescription -> LocalBuildInfo
                   -> UserHooks -> InstallFlags -> IO ()
installHookWithExtraGhciLibraries pkg_descr localbuildinfo _ flags = do
  let copyFlags = defaultCopyFlags {
                      copyDistPref   = installDistPref flags,
                      copyDest       = toFlag NoCopyDest,
                      copyVerbosity  = installVerbosity flags
                  }
  install pkg_descr localbuildinfo copyFlags
  let registerFlags = defaultRegisterFlags {
                          regDistPref  = installDistPref flags,
                          regInPlace   = installInPlace flags,
                          regPackageDB = installPackageDB flags,
                          regVerbosity = installVerbosity flags
                      }
  when (hasLibs pkg_descr) $ register' pkg_descr localbuildinfo registerFlags

regHookWithExtraGhciLibraries :: PackageDescription -> LocalBuildInfo
        -> UserHooks -> RegisterFlags -> IO ()
regHookWithExtraGhciLibraries pkg_descr localbuildinfo _ flags =
    if hasLibs pkg_descr
    then register' pkg_descr localbuildinfo flags
    else setupMessage verbosity
           "Package contains no library to register:" (packageId pkg_descr)
  where verbosity = fromFlag (regVerbosity flags)
  
register' :: PackageDescription -> LocalBuildInfo
          -> RegisterFlags -- ^Install in the user's database?; verbose
          -> IO ()
register' pkg@PackageDescription { library       = Just lib  }
          lbi@LocalBuildInfo     { libraryConfig = Just clbi } regFlags
  = do

    installedPkgInfoRaw <- generateRegistrationInfo
                           verbosity pkg lib lbi clbi inplace distPref

    let ghciLibraries    = case lookup "x-extra-ghci-libraries" (customFieldsBI (libBuildInfo lib)) of
                             Nothing -> []
                             Just s  -> [s]
        installedPkgInfo = installedPkgInfoRaw {
                                extraGHCiLibraries = ghciLibraries }

     -- Three different modes:
    case () of
     _ | modeGenerateRegFile   -> writeRegistrationFile installedPkgInfo
       | modeGenerateRegScript -> die "Generate Reg Script not supported"
       | otherwise             -> registerPackage verbosity
                                    installedPkgInfo pkg lbi inplace
                                    (withPackageDB lbi)

  where
    modeGenerateRegFile = isJust (flagToMaybe (regGenPkgConf regFlags))
    modeGenerateRegScript = fromFlag (regGenScript regFlags)
    inplace   = fromFlag (regInPlace regFlags)
    packageDb = nub $ withPackageDB lbi ++
                      maybeToList (flagToMaybe  (regPackageDB regFlags))
    distPref  = fromFlag (regDistPref regFlags)
    verbosity = fromFlag (regVerbosity regFlags)
    regFile             = fromMaybe (display (packageId pkg) <.> "conf")
                                    (fromFlag (regGenPkgConf regFlags))
    writeRegistrationFile installedPkgInfo = do
      notice verbosity ("Creating package registration file: " ++ regFile)
      writeUTF8File regFile (showInstalledPackageInfo installedPkgInfo)

register' _ _ regFlags = notice verbosity "No package to register"
  where
    verbosity = fromFlag (regVerbosity regFlags)
