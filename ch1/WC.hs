-- Woo! This is kinda boring, but we'll get to good stuff soon.

main = interact wordCount
--  where wordCount input = show (length (lines input)) ++ "\n"
--  where wordCount input = show (length (words input)) ++ "\n"
  where wordCount input = show (length input) ++ "\n"
