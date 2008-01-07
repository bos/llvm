module DiffFFI (main) where

import Control.Monad (forM_)
import Data.List (foldl')
import qualified Data.Set as S
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Regex.Posix ((=~))

import FunctionMangulation (pattern)

functionSet :: String -> String -> S.Set String
functionSet pat s = foldl' go S.empty (s =~ pat)
  where go set (_:_:name:_) = S.insert ("LLVM" ++ name) set
        go set _ = set

cFunctions :: String -> S.Set String
cFunctions = functionSet pattern

hsFunctions :: String -> S.Set String
hsFunctions = functionSet "\"(LLVM)([a-zA-Z0-9_]+)(\")"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [cFile, hsFile] -> do
              c <- cFunctions `fmap` readFile cFile
              hs <- hsFunctions `fmap` readFile hsFile
              putStrLn "In C, not Haskell:"
              forM_ (S.toAscList $ S.difference c hs) $ putStrLn . ("  "++)
              putStrLn "In Haskell, not C:"
              forM_ (S.toAscList $ S.difference hs c) $ putStrLn . ("  "++)
    _ -> do
         hPutStrLn stderr "Usage: "
         exitFailure
