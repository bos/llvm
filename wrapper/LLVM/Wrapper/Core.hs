module LLVM.Wrapper.Core
    (
    -- ** Modules
      moduleCreateWithName
    , disposeModule
    , withModule
    ) where

import Foreign.C.String
import Control.Exception

import LLVM.FFI.Core
    ( ModuleRef
    , disposeModule
    )
import qualified LLVM.FFI.Core as FFI

moduleCreateWithName :: String -> IO ModuleRef
moduleCreateWithName name = withCAString name FFI.moduleCreateWithName

withModule :: String -> (ModuleRef -> IO a) -> IO a
withModule n f = do m <- moduleCreateWithName n
                    finally (f m) (disposeModule m)
