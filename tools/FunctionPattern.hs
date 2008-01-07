module FunctionPattern (pattern) where

pattern :: String
pattern = "^([A-Za-z0-9_ ]+ ?\\*?)[ \t\n]*" ++
          "LLVM([A-Za-z0-9_]+)\\(([a-zA-Z0-9_*, \t\n]+)\\);"
