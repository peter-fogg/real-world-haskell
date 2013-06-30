-- Exception on [x] or []! Not sure what the idiomatic way to handle this is.
lastButOne :: [a] -> a
lastButOne (x:y:[]) = x
lastButOne (x:xs) = lastButOne xs
