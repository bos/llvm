-- |Type level decimal numbers.
module Data.TypeNumbers(
           IsTypeNumber, typeNumber,
	   D0(..),D1(..),D2(..),D3(..),D4(..),D5(..),D6(..),D7(..),D8(..),D9(..),
	   End(..)
	   ) where

-- |A type level number, i.e., a sequence of type level digits.
class IsTypeNumber ds where
    typeNumber' :: (Num a) => ds -> a -> a

-- |Get the numeric value of a type level number.
-- This function does not evaluate its argument.
typeNumber :: (IsTypeNumber ds, Num a) => ds -> a
typeNumber ds = typeNumber' ds 0

-- |Mark the end of a digit sequence.
data End = End
instance IsTypeNumber End where
     typeNumber' _ a = a

-- |The 'D0' - 'D9' types represent type level digits that form
-- a number by, e.g, @D1 (D0 (D5 End))@.
-- On the value level a slightly more palatable form can be used,
-- @D1$D0$D5$End@.
data D0 a = D0 a
data D1 a = D1 a
data D2 a = D2 a
data D3 a = D3 a
data D4 a = D4 a
data D5 a = D5 a
data D6 a = D6 a
data D7 a = D7 a
data D8 a = D8 a
data D9 a = D9 a

instance (IsTypeNumber ds) => IsTypeNumber (D0 ds) where
    typeNumber' ~(D0 ds) acc = typeNumber' ds (10*acc + 0)
instance (IsTypeNumber ds) => IsTypeNumber (D1 ds) where
    typeNumber' ~(D1 ds) acc = typeNumber' ds (10*acc + 1)
instance (IsTypeNumber ds) => IsTypeNumber (D2 ds) where
    typeNumber' ~(D2 ds) acc = typeNumber' ds (10*acc + 2)
instance (IsTypeNumber ds) => IsTypeNumber (D3 ds) where
    typeNumber' ~(D3 ds) acc = typeNumber' ds (10*acc + 3)
instance (IsTypeNumber ds) => IsTypeNumber (D4 ds) where
    typeNumber' ~(D4 ds) acc = typeNumber' ds (10*acc + 4)
instance (IsTypeNumber ds) => IsTypeNumber (D5 ds) where
    typeNumber' ~(D5 ds) acc = typeNumber' ds (10*acc + 5)
instance (IsTypeNumber ds) => IsTypeNumber (D6 ds) where
    typeNumber' ~(D6 ds) acc = typeNumber' ds (10*acc + 6)
instance (IsTypeNumber ds) => IsTypeNumber (D7 ds) where
    typeNumber' ~(D7 ds) acc = typeNumber' ds (10*acc + 7)
instance (IsTypeNumber ds) => IsTypeNumber (D8 ds) where
    typeNumber' ~(D8 ds) acc = typeNumber' ds (10*acc + 8)
instance (IsTypeNumber ds) => IsTypeNumber (D9 ds) where
    typeNumber' ~(D9 ds) acc = typeNumber' ds (10*acc + 9)
