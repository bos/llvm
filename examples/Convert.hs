{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances #-}
module Convert(Convert(..)) where
import Data.Int
import Data.Word
import Foreign.Ptr (FunPtr)

type Importer f = FunPtr f -> f

class Convert f where
    convert :: Importer f

foreign import ccall safe "dynamic" c_IOFloat :: Importer (IO Float)
instance Convert (IO Float) where convert = c_IOFloat

foreign import ccall safe "dynamic" c_Float_IOFloat :: Importer (Float -> IO Float)
instance Convert (Float -> IO Float) where convert = c_Float_IOFloat

foreign import ccall safe "dynamic" c_Float_Float :: Importer (Float -> Float)
instance Convert (Float -> Float) where convert = c_Float_Float

foreign import ccall safe "dynamic" c_IODouble :: Importer (IO Double)
instance Convert (IO Double) where convert = c_IODouble

foreign import ccall safe "dynamic" c_Double_IODouble :: Importer (Double -> IO Double)
instance Convert (Double -> IO Double) where convert = c_Double_IODouble

foreign import ccall safe "dynamic" c_Double_Double :: Importer (Double -> Double)
instance Convert (Double -> Double) where convert = c_Double_Double

foreign import ccall safe "dynamic" c_Word32_IOWord32 :: Importer (Word32 -> IO Word32)
instance Convert (Word32 -> IO Word32) where convert = c_Word32_IOWord32

foreign import ccall safe "dynamic" c_Word32_Word32 :: Importer (Word32 -> Word32)
instance Convert (Word32 -> Word32) where convert = c_Word32_Word32

foreign import ccall safe "dynamic" c_Int32_IOInt32 :: Importer (Int32 -> IO Int32)
instance Convert (Int32 -> IO Int32) where convert = c_Int32_IOInt32

foreign import ccall safe "dynamic" c_Int32_Int32 :: Importer (Int32 -> Int32)
instance Convert (Int32 -> Int32) where convert = c_Int32_Int32

