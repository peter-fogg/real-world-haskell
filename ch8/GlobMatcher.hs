-- Doesn't handle escaping now -- may add later.
module GlobMatcher
       (matchesGlob)
       where

data Pattern = Char Char
             | Wildcard
             | Question
             | IncludeClass String
             | ExcludeClass String
             deriving (Show, Eq)

matchesGlob :: String -> String -> Either GlobError Bool
matchesGlob pat name = fmap (\ p -> matchesGlob' p name) (parsePattern pat)

matchesGlob' :: [Pattern] -> String -> Bool
matchesGlob' [] []                          = True
matchesGlob' [Wildcard] []                  = True
matchesGlob' _ []                           = False
matchesGlob' [] _                           = False
matchesGlob' ((Char c):ps) (x:xs) | c == x = matchesGlob' ps xs
                                  | otherwise = False
matchesGlob' (Question:ps) (x:xs)           = matchesGlob' ps xs
matchesGlob' pat@(Wildcard:ps) s@(x:xs)     = matchesGlob' pat xs
                                              || matchesGlob' ps xs
                                              || matchesGlob' ps s
matchesGlob' ((IncludeClass cls):ps) (x:xs) = if x `elem` cls then matchesGlob' ps xs else False
matchesGlob' ((ExcludeClass cls):ps) (x:xs) = if x `elem` cls then False else matchesGlob' ps xs

type GlobError = String

parsePattern :: String -> Either GlobError [Pattern]
parsePattern []          = Right []
parsePattern ('*':ps)    = fmap (Wildcard:) (parsePattern ps)
parsePattern ('?':ps)    = fmap (Question:) (parsePattern ps)
parsePattern ('\\':c:ps) = fmap (\ p -> (Char c):p) (parsePattern ps)
parsePattern ('[':c:ps)  = case c of
  ']' -> Left "char class must contain characters"
  '^' -> case parseCharClass ps of -- There is almost certainly a neater way
    Left err          -> Left err  -- to do this, but I don't know it.
    Right (cls, rest) -> fmap (\ p -> (ExcludeClass cls):p) $ parsePattern rest
  c   -> case parseCharClass (c:ps) of
    Left err          -> Left err
    Right (cls, rest) -> fmap (\ p -> (IncludeClass cls):p) $ parsePattern rest
parsePattern (c:ps)      = fmap ((Char c):) (parsePattern ps)

parseCharClass :: String -> Either GlobError (String, String)
parseCharClass []       = Left "unterminated char class"
parseCharClass (']':ps) = Right ("", ps)
parseCharClass (p:ps)   = fmap build (parseCharClass ps)
  where build (pat, rest) = (p:pat, rest)
