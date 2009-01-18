{-# LANGUAGE ScopedTypeVariables #-}
-- These are replacements for the broken equivalents in Foreign.*.
-- The functions in Foreign.* do not obey the required alignment.
module LLVM.Util.Foreign where

import Foreign.Ptr(alignPtr, Ptr)
import Foreign.Storable(Storable(poke, sizeOf, alignment))
import Foreign.Marshal.Alloc(allocaBytes)
import Foreign.Marshal.Array(allocaArray, pokeArray)

with :: Storable a => a -> (Ptr a -> IO b) -> IO b
with x act =
    alloca $ \ p -> do
    poke p x
    act p

alloca :: forall a b . Storable a => (Ptr a -> IO b) -> IO b
alloca act =
    allocaBytes (2 * sizeOf (undefined :: a)) $ \ p ->
       act $ alignPtr p (alignment (undefined :: a))

withArrayLen :: (Storable a) => [a] -> (Int -> Ptr a -> IO b) -> IO b
withArrayLen xs act =
    let l = length xs in
    allocaArray (l+1) $ \ p -> do
    let p' = alignPtr p (alignment (head xs))
    pokeArray p' xs
    act l p'

