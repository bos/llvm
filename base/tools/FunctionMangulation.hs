module FunctionMangulation
    (
      pattern
    , rewrite
    , rewriteFunction
    ) where

import Control.Monad (forM)
import Data.Char (isSpace, toLower)
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import Text.Regex.Posix ((=~), (=~~))

pattern :: String
pattern = "^([A-Za-z0-9_ ]+ ?\\*?)[ \t\n]*" ++
          "LLVM([A-Za-z0-9_]+)\\(([a-zA-Z0-9_*, \t\n]+)\\);"

dropSpace :: String -> String
dropSpace = dropWhile isSpace

renameType :: String -> String
renameType t | "LLVM" `isPrefixOf` t = rename' (drop 4 t)
             | otherwise = rename' t
  where rename' "int" = "CInt"
        rename' "unsigned" = "CUInt"
        rename' "long long" = "CLLong"
        rename' "unsigned long long" = "CULLong"
        rename' "void" = "()"
        rename' "const char *" = "CString"
        rename' "char *" = "CString"
        rename' s | "*" `isSuffixOf` s = pointer s
                  | otherwise = strip s
        pointer p = case reverse p of
                      ('*':ps) -> "(Ptr " ++ rename' (reverse ps) ++ ")"
                      _ -> p

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = case break p xs of
               (h,(_:t)) -> h : split p t
               (s,_) -> [s]

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropSpace

dropName :: String -> String
dropName s =
    case s =~ "^((const )?[A-Za-z0-9_]+( \\*+)?) ?[A-Za-z0-9]*$" of
      ((_:typ:_):_) -> typ
      _ -> "{- oops! -} " ++ s

rewriteFunction :: String -> String -> String -> String
rewriteFunction cret cname cparams =
    let ret = "IO " ++ renameType (strip cret)
        params = map renameParam . split (==',') $ cparams
	params' = if params == ["()"] then [] else params
        name = let (n:ame) = cname in toLower n : ame
    in foreign ++ "\"LLVM" ++ cname ++ "\" " ++ name ++
           "\n    :: " ++ intercalate " -> " (params' ++ [ret])
  where renameParam = renameType . dropName . strip
        foreign = "foreign import ccall unsafe "
    
rewrite :: Monad m => String -> m [String]
rewrite s = do
    matches <- s =~~ pattern
    forM matches $ \(_:cret:cname:cparams:_) ->
         return (rewriteFunction cret cname cparams)
