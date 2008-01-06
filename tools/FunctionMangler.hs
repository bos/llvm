module FunctionMangler where

import Control.Monad (forM)
import Data.Char (isSpace, toLower)
import Data.List (intercalate, isPrefixOf)
import Text.Regex.Posix ((=~), (=~~))

dropSpace :: String -> String
dropSpace = dropWhile isSpace

renameType :: String -> String
renameType "int" = "CInt"
renameType "unsigned" = "CUInt"
renameType "void" = "()"
renameType "const char *" = "CString"
renameType s | "LLVM" `isPrefixOf` s = pointer $ drop 4 s
             | otherwise = error $ "cannot handle " ++ show s
    where pointer p = case reverse p of
                        ('*':ps) -> ("Ptr "++) . reverse $ dropSpace ps
                        _ -> p

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = case break p xs of
               (h,(_:t)) -> h : split p t
               (s,_) -> [s]

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropSpace

dropName :: String -> String
dropName s =
    let pattern = "^((const )?[A-Za-z0-9_]+( \\*)?) ?[A-Za-z0-9]*$"
        ((_:typ:_):_) = s =~ pattern
    in typ

rewrite :: Monad m => String -> m [String]
rewrite s = do
    let pattern = "^([A-Za-z0-9_ ]+ ?\\*?)[ \t\n]*" ++
                  "LLVM([A-Za-z0-9_]+)\\(([a-zA-Z0-9_*, \t\n]+)\\);"
    matches <- s =~~ pattern
    forM matches $ \(_:cret:cname:cparams:_) -> do
      let ret = "IO " ++ renameType (strip cret)
          params = map renameParam . split (==',') $ cparams
          name = let (n:ame) = cname in toLower n : ame
      return $ foreign ++ "\"LLVM" ++ cname ++ "\" " ++ name ++
               "\n    :: " ++ intercalate " -> " (params ++ [ret])
  where renameParam = renameType . dropName . strip
        foreign = "foreign import ccall unsafe "

main :: IO ()
main = interact (intercalate "\n\n" . concat . rewrite) >> putStr "\n"
