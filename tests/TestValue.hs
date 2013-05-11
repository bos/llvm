module TestValue (main) where

import qualified LLVM.Core as Core
import qualified LLVM.Core.Type as T
import qualified LLVM.Core.Value as V

testArguments :: (T.DynamicType r, T.Params p, V.Params p v, V.Value v)
                 => T.Module -> String -> IO (V.Function r p)
testArguments m name = do
  func <- Core.addFunction m name (T.function undefined undefined)
  V.dumpValue func
  let arg = V.params func
  V.dumpValue arg
  return func

voidArguments :: T.Module -> IO ()
voidArguments m = do
  func <- Core.addFunction m "void" (T.function (undefined :: T.Void) ())
  V.dumpValue func
  return ()

type F a = V.Function a a
type P a = V.Function (T.Pointer a) (T.Pointer a)
type V a = V.Function (T.Vector a) (T.Vector a)

arguments :: T.Module -> IO ()
arguments m = do
  voidArguments m

  testArguments m "int1" :: IO (F T.Int1)
  testArguments m "int8" :: IO (F T.Int8)
  testArguments m "int16" :: IO (F T.Int16)
  testArguments m "int32" :: IO (F T.Int32)
  testArguments m "int64" :: IO (F T.Int64)
  testArguments m "float" :: IO (F T.Float)
  testArguments m "double" :: IO (F T.Double)
  testArguments m "float128" :: IO (F T.Float128)
  testArguments m "x86Float80" :: IO (F T.X86Float80)
  testArguments m "ppcFloat128" :: IO (F T.PPCFloat128)

  testArguments m "ptrInt1" :: IO (P T.Int1)
  testArguments m "ptrInt8" :: IO (P T.Int8)
  testArguments m "ptrInt16" :: IO (P T.Int16)
  testArguments m "ptrInt32" :: IO (P T.Int32)
  testArguments m "ptrInt64" :: IO (P T.Int64)
  testArguments m "ptrFloat" :: IO (P T.Float)
  testArguments m "ptrDouble" :: IO (P T.Double)
  testArguments m "ptrFloat128" :: IO (P T.Float128)
  testArguments m "ptrX86Float80" :: IO (P T.X86Float80)
  testArguments m "ptrPpcFloat128" :: IO (P T.PPCFloat128)

  testArguments m "vecInt1" :: IO (V T.Int1)
  testArguments m "vecInt8" :: IO (V T.Int8)
  testArguments m "vecInt16" :: IO (V T.Int16)
  testArguments m "vecInt32" :: IO (V T.Int32)
  testArguments m "vecInt64" :: IO (V T.Int64)
  testArguments m "vecFloat" :: IO (V T.Float)
  testArguments m "vecDouble" :: IO (V T.Double)
  testArguments m "vecFloat128" :: IO (V T.Float128)
  testArguments m "vecX86Float80" :: IO (V T.X86Float80)
  testArguments m "vecPpcFloat128" :: IO (V T.PPCFloat128)

  return ()

main :: IO ()
main = do
  m <- Core.createModule "m"
  arguments m
  return ()
