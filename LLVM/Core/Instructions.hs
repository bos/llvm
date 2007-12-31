module LLVM.Core.Instructions
    (
      IntPredicate(..)
    , RealPredicate(..)
    , fromIP
    , fromRP
    ) where

import Foreign.C.Types (CInt)

data IntPredicate =
    IntEQ                       -- ^ equal
  | IntNE                       -- ^ not equal
  | IntUGT                      -- ^ unsigned greater than
  | IntUGE                      -- ^ unsigned greater or equal
  | IntULT                      -- ^ unsigned less than
  | IntULE                      -- ^ unsigned less or equal
  | IntSGT                      -- ^ signed greater than
  | IntSGE                      -- ^ signed greater or equal
  | IntSLT                      -- ^ signed less than
  | IntSLE                      -- ^ signed less or equal
    deriving (Eq, Ord, Enum, Show)

fromIP :: IntPredicate -> CInt
fromIP ip = fromIntegral (fromEnum ip + 32)

data RealPredicate =
    RealFalse           -- ^ Always false (always folded)
  | RealOEQ             -- ^ True if ordered and equal
  | RealOGT             -- ^ True if ordered and greater than
  | RealOGE             -- ^ True if ordered and greater than or equal
  | RealOLT             -- ^ True if ordered and less than
  | RealOLE             -- ^ True if ordered and less than or equal
  | RealONE             -- ^ True if ordered and operands are unequal
  | RealORD             -- ^ True if ordered (no nans)
  | RealUNO             -- ^ True if unordered: isnan(X) | isnan(Y)
  | RealUEQ             -- ^ True if unordered or equal
  | RealUGT             -- ^ True if unordered or greater than
  | RealUGE             -- ^ True if unordered, greater than, or equal
  | RealULT             -- ^ True if unordered or less than
  | RealULE             -- ^ True if unordered, less than, or equal
  | RealUNE             -- ^ True if unordered or not equal
  | RealT               -- ^ Always true (always folded)
    deriving (Eq, Ord, Enum, Show)

fromRP :: RealPredicate -> CInt
fromRP = fromIntegral . fromEnum
