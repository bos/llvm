module FunctionMangler (main) where

import Data.List (intercalate)

import FunctionMangulation (rewrite)

main :: IO ()
main = interact (intercalate "\n\n" . concat . rewrite) >> putStr "\n"
