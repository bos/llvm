--module FunctionMangler (main) where

import Control.Monad (forM)
import Data.Char (isSpace, toLower)
import Data.List (intercalate, isPrefixOf)
import Text.Regex.Posix ((=~), (=~~))

renameType :: String -> String
renameType "int" = "CInt"
renameType "unsigned" = "CUInt"
renameType "void" = "()"
renameType "const char *" = "CString"
renameType s | "LLVM" `isPrefixOf` s = drop 4 s
             | otherwise = error $ "cannot handle " ++ s

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = case break p xs of
               (h,(_:t)) -> h : split p t
               (s,_) -> [s]

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

dropName :: String -> String
dropName s =
    let ((_:typ:_):_) = s =~ "^((const )?[A-Za-z0-9_]+( \\*)?) ?[A-Za-z0-9]*$"
    in typ

rewrite :: String -> [[String]]
rewrite s = do
  let pat = "^([A-Za-z0-9_ ]+ ?\\*?)[ \t\n]*LLVM([A-Za-z0-9_]+)\\(([a-zA-Z0-9_*, \t\n]+)\\);"
  matches <- s =~~ pat
  forM matches $ \(_:ret:name:params:_) -> do
    let retType = "IO " ++ renameType ret
        ps = map (renameType . dropName . strip) . split (==',') $ params
        hsName = let (n:ame) = name in toLower n : ame
    return $ "foreign import ccall unsafe \"LLVM" ++ name ++ "\" " ++ hsName ++
             "\n    :: " ++ intercalate " -> " (ps ++ [retType])

main :: IO ()
main = interact (intercalate "\n\n" . concat . rewrite) >> putStr "\n"
