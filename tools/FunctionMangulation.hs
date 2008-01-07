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
renameType "int" = "CInt"
renameType "unsigned" = "CUInt"
renameType "void" = "()"
renameType "const char *" = "CString"
renameType s | "LLVM" `isPrefixOf` s = pointer $ drop 4 s
             | "*" `isSuffixOf` s = pointer s
             | otherwise = error $ "cannot handle " ++ show s
    where pointer p = case reverse p of
                        ('*':ps) -> "(Ptr " ++ pointer (reverse ps) ++ ")"
                        _ -> dropSpace p

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
      _ -> error $ "dropName: " ++ show s

rewriteFunction :: String -> String -> String -> String
rewriteFunction cret cname cparams =
    let ret = "IO " ++ renameType (strip cret)
        params = map renameParam . split (==',') $ cparams
        name = let (n:ame) = cname in toLower n : ame
    in foreign ++ "\"LLVM" ++ cname ++ "\" " ++ name ++
           "\n    :: " ++ intercalate " -> " (params ++ [ret])
  where renameParam = renameType . dropName . strip
        foreign = "foreign import ccall unsafe "
    
rewrite :: Monad m => String -> m [String]
rewrite s = do
    matches <- s =~~ pattern
    forM matches $ \(_:cret:cname:cparams:_) ->
         return (rewriteFunction cret cname cparams)
