module LLVM.Core.Utils
    (
      defineGlobal
    , declareFunction
    , defineFunction
    ) where

import Prelude hiding (mod)

import qualified LLVM.Core as Core
import qualified LLVM.Core.Builder as B
import qualified LLVM.Core.Constant as C
import qualified LLVM.Core.Type as T
import qualified LLVM.Core.Value as V


defineGlobal :: (V.ConstValue a, V.TypedValue a t) => T.Module -> String -> a
             -> IO (V.GlobalVar t)
defineGlobal mod name val = do
  global <- Core.addGlobal mod (V.typeOf val) name
  Core.setInitializer global val
  return global

declareFunction :: T.Params p => T.Module -> String -> T.Function p
                -> IO (V.Function p)
declareFunction mod name typ = do
  maybeFunc <- Core.getNamedFunction mod name
  case maybeFunc of
    Nothing -> Core.addFunction mod name typ
    Just func -> return $ let t = V.typeOf func
                          in if T.elementTypeDyn t /= T.toAnyType typ
                             then C.bitCast func (T.pointer typ)
                             else func

defineFunction :: T.Params p => T.Module -> String -> T.Function p
               -> IO (V.Function p, B.BasicBlock)
defineFunction mod name typ = do
  func <- Core.addFunction mod name typ
  bblk <- Core.appendBasicBlock func "entry"
  return (func, bblk)
