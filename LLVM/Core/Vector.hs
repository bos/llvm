{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, ScopedTypeVariables #-}
module LLVM.Core.Vector(MkVector(..)) where
import Data.TypeNumbers
import LLVM.Core.Type
import LLVM.Core.Data
import LLVM.Core.CodeGen(IsConst(..), ConstValue(..))
import LLVM.FFI.Core(constVector)
import Foreign.Ptr(Ptr, castPtr)
import Foreign.Storable(Storable(..))
import Foreign.Marshal.Array(peekArray, pokeArray, withArrayLen)
import System.IO.Unsafe(unsafePerformIO)

-- XXX Should these really be here?
class (IsPowerOf2 n, IsPrimitive a) => MkVector va n a | va -> n a, n a -> va where
    mkVector :: va -> Vector n a

{-
instance (IsPrimitive a) => MkVector (Value a) (D1 End) (Value a) where
    mkVector a = Vector [a]
-}

instance (IsPrimitive a) => MkVector (a, a) (D2 End) a where
    mkVector (a1, a2) = Vector [a1, a2]

instance (IsPrimitive a) => MkVector (a, a, a, a) (D4 End) a where
    mkVector (a1, a2, a3, a4) = Vector [a1, a2, a3, a4]

instance (IsPrimitive a) => MkVector (a, a, a, a, a, a, a, a) (D8 End) a where
    mkVector (a1, a2, a3, a4, a5, a6, a7, a8) = Vector [a1, a2, a3, a4, a5, a6, a7, a8]

instance (Storable a, IsTypeNumber n) => Storable (Vector n a) where
    sizeOf _ = sizeOf (undefined :: a) * typeNumber (undefined :: n)
    alignment _ = alignment (undefined :: a) * typeNumber (undefined :: n)
    peek p = fmap Vector $ peekArray (typeNumber (undefined :: n)) (castPtr p :: Ptr a)
    poke p (Vector vs) = pokeArray (castPtr p :: Ptr a) vs

instance (IsPowerOf2 n, IsPrimitive a, IsConst a) => IsConst (Vector n a) where
    constOf (Vector vs) =
        unsafePerformIO $
        withArrayLen [ c | v <- vs, let ConstValue c = constOf v ]  $ \ len ptr ->
        return $ ConstValue $ constVector ptr (fromIntegral len)
