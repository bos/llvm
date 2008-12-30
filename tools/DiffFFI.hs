module DiffFFI (main) where

import Control.Monad (forM_)
import Data.List (foldl')
import qualified Data.Map as M
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Regex.Posix ((=~))

import FunctionMangulation (pattern, rewriteFunction)

cFunctions :: String -> M.Map String String
cFunctions s = foldl' go M.empty (s =~ pattern)
  where go m (_:ret:name:params:_) =
            M.insert ("LLVM" ++ name) (rewriteFunction ret name params) m
        go m _ = m

hsFunctions :: String -> M.Map String String
hsFunctions s = foldl' go M.empty (s =~ pat)
    where pat = "\"([a-zA-Z0-9_]+)\"[ \t\n]+([a-zA-Z0-9_']+)"
          go m (_:cname:hsname:_) = M.insert cname hsname m
          go m _ = m

main :: IO ()
main = do
  args <- getArgs
  case args of
    [cFile, hsFile] -> do
              c <- cFunctions `fmap` readFile cFile
              hs <- hsFunctions `fmap` readFile hsFile
              putStrLn "In C, not Haskell:"
              forM_ (M.toAscList $ M.difference c hs) $ \(_, hsfunc) ->
                    putStrLn hsfunc
              putStrLn "In Haskell, not C:"
              forM_ (M.keys $ M.difference hs c) $ putStrLn . ("  "++)
    _ -> do
         hPutStrLn stderr "Usage: DiffFFI cFile hsFile"
         exitFailure
