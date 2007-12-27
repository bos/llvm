module LLVM.Instances () where

import LLVM.Internal (Type(..))

instance Eq Type where
    a == b = fromType a == fromType b
