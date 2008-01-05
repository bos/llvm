module IntrinsicMangler (main) where

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as C
import Data.Maybe (catMaybes)
import Text.Regex.Posix ((=~~))

maybeName :: C.ByteString -> Maybe C.ByteString
maybeName line = do
  ((_:name:_):_) <- line =~~ "^[ \t]*([a-z0-9_]+),[ \t]*//[ \t]*llvm\\."
  return name

main :: IO ()
main = do
  input <- (catMaybes . map maybeName . C.lines) `fmap` C.getContents

  putStrLn "-- automatically generated file - do not edit!"
  putStrLn "module LLVM.Core.Intrinsics (Intrinsic(..)) where"
  putStrLn "data Intrinsic ="
  putStrLn "      NotIntrinsic"
  forM_ input $ C.putStrLn . (C.append (C.pack "    | I_"))
  putStrLn "    deriving (Eq, Ord, Enum, Show)"
