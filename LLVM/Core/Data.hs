module LLVM.Core.Data(IntN(..), WordN(..), FP128(..),
       		      Array(..), Vector(..), Ptr(..)) where
import Data.TypeNumbers

-- TODO:
-- Make instances IntN, WordN to actually do the right thing.
-- Make FP128 do the right thing
-- Make Array functions.

-- |Variable sized signed integer.
-- The /n/ parameter should belong to @IsTypeNumber@.
newtype (IsTypeNumber n) => IntN n = IntN Integer

-- |Variable sized unsigned integer.
-- The /n/ parameter should belong to @IsTypeNumber@.
newtype (IsTypeNumber n) => WordN n = WordN Integer

-- |128 bit floating point.
newtype FP128 = FP128 Rational

-- |Fixed sized arrays, the array size is encoded in the /n/ parameter.
newtype (IsTypeNumber n) => Array n a = Array [a]

-- XXX Power of 2 size constraint not enforced
-- |Fixed sized vector, the array size is encoded in the /n/ parameter.
newtype Vector n a = Vector (Array n a)

-- |Pointer type.
newtype Ptr a = Ptr a
