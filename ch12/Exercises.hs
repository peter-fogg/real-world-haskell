f4 :: (a, a, a, a) -> Int -> a
f4 (ele, _, _, _) 0 = ele
f4 (_, ele, _, _) 1 = ele
f4 (_, _, ele, _) 2 = ele
f4 (_, _, _, ele) 3 = ele
f4 _ _              = error "nope"

f6 :: (a, a, a, a, a, a) -> Int -> a
f6 (ele, _, _, _, _, _) 0 = ele
f6 (_, ele, _, _, _, _) 1 = ele
f6 (_, _, ele, _, _, _) 2 = ele
f6 (_, _, _, ele, _, _) 3 = ele
f6 (_, _, _, ele, _, _) 4 = ele
f6 (_, _, _, ele, _, _) 5 = ele
f6 _ _                    = error "nope"

-- Well that was dumb.
