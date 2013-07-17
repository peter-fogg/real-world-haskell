getRandom :: Random a => RandomState (a, a)
getRandom = do
  get <- get
  let (val, gen') = random gen
  put gen'
  return val
