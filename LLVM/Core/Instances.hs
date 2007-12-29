module LLVM.Core.Instances () where

import LLVM.Core.Types (Type(..), AnyType)

instance Eq AnyType where
    a == b = fromType a == fromType b
