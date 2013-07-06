module GlobRegex
       (
         globToRegex
       , matchesGlob
       , matchesGlobCase
       ) where
import Text.Regex.Posix ((=~))
import Data.Char (toLower)

globToRegex :: String -> String
globToRegex cs = '^': globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' []             = []
globToRegex' ('*':cs)       = ".*" ++ globToRegex' cs
globToRegex' ('*':'*':cs)   = ".*/" ++ globToRegex' cs -- eh
globToRegex' ('?':cs)       = '.': globToRegex' cs
globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs)     = '['  :  c : charClass cs
globToRegex' ('[':_)        = error "unterminated character class"
globToRegex' (c:cs)         = escape c ++ globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"

charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = name =~ globToRegex pat

globToRegexCase :: String -> Bool -> String
globToRegexCase cs True  = '^': globToRegex' (map toLower cs) ++ "$"
globToRegexCase cs False = '^': globToRegex' cs ++ "$"

matchesGlobCase :: FilePath -> String -> Bool -> Bool
matchesGlobCase name pat True  = map toLower name =~ globToRegex pat
matchesGlobCase name pat False = name =~ globToRegex pat

type GlobError = String

globToRegexEither :: String -> Either GlobError String
globToRegexEither cs = fmap enclose $ globToRegexEither' cs
  where enclose regex = '^': regex ++ "$"

globToRegexEither' :: String -> Either GlobError String
globToRegexEither' []             = Right []
globToRegexEither' ('*':cs)       = fmap (".*"++) $ Right cs
globToRegexEither' ('?':cs)       = fmap ('.':) $ Right cs
globToRegexEither' ('[':'!':c:cs) = fmap (\s -> "[^"++c:s) $ charClassEither cs
globToRegexEither' ('[':c:cs)     = fmap (\s -> '[':c:s) $ charClassEither cs
globToRegexEither' ('[':_)        = Left "unterminated character class"
globToRegexEither' (c:cs)         = fmap ((escape c)++) $ globToRegexEither cs

charClassEither :: String -> Either GlobError String
charClassEither (']':cs) = fmap (']':) $ globToRegexEither' cs
charClassEither (c:cs)   = fmap (c:) $ charClassEither cs
charClassEither []       = Left "unterminated character class"