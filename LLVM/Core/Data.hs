{-# LANGUAGE EmptyDataDecls, DeriveDataTypeable #-}
module LLVM.Core.Data(IntN(..), WordN(..), FP128(..),
       Array(..), Vector(..), Ptr, Label, Struct(..), PackedStruct(..)) where
import Data.Typeable
import Foreign.Ptr(Ptr)

-- TODO:
-- Make instances IntN, WordN to actually do the right thing.
-- Make FP128 do the right thing
-- Make Array functions.

-- |Variable sized signed integer.
-- The /n/ parameter should belong to @PosI@.
newtype IntN n = IntN Integer
    deriving (Show, Typeable)

-- |Variable sized unsigned integer.
-- The /n/ parameter should belong to @PosI@.
newtype WordN n = WordN Integer
    deriving (Show, Typeable)

-- |128 bit floating point.
newtype FP128 = FP128 Rational
    deriving (Show, Typeable)

-- |Fixed sized arrays, the array size is encoded in the /n/ parameter.
newtype Array n a = Array [a]
    deriving (Show, Typeable)

-- |Fixed sized vector, the array size is encoded in the /n/ parameter.
newtype Vector n a = Vector [a]
    deriving (Show, Typeable)

-- |Label type, produced by a basic block.
data Label
    deriving (Typeable)

-- |Struct types; a list (nested tuple) of component types.
newtype Struct a = Struct a
    deriving (Show, Typeable)
newtype PackedStruct a = PackedStruct a
    deriving (Show, Typeable)
