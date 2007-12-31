module LLVM.Core.Instructions
    (
      RealPredicate(..)
    , fromRP

    -- * Constant expressions
    , neg
    , not
    , fcmp
    ) where

import Foreign.C.Types (CInt)
import Prelude hiding (not)

import qualified LLVM.Core.FFI as FFI
import qualified LLVM.Core.Types as T
import qualified LLVM.Core.Values as V


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

neg :: (V.ConstValue v, T.Arithmetic t) => v -> V.ConstExpr t
neg = V.ConstExpr . V.mkAnyValue . FFI.constNeg . V.valueRef

not :: (V.ConstValue v, T.Integer t) => v -> V.ConstExpr t
not = V.ConstExpr . V.mkAnyValue . FFI.constNot . V.valueRef

fcmp :: T.Real t => RealPredicate -> V.ConstReal t -> V.ConstReal t
     -> V.ConstExpr T.Int1
fcmp p a b = V.ConstExpr . V.mkAnyValue $
             FFI.constFCmp (fromRP p) (V.valueRef a) (V.valueRef b)
