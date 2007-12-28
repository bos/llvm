module LLVM.Core.Instances () where

import LLVM.Core.Types (Type(..))

instance Eq Type where
    a == b = fromType a == fromType b
