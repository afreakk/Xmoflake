module XmobarUtils (xmobarShorten) where

xmobarShorten :: Int -> String -> String
xmobarShorten maxLen = shorten False "" 0
  where
    shorten :: Bool -> String -> Int -> String -> String
    shorten isTag str idx ('<':xs) = shorten True  ('<':str) idx    xs
    shorten isTag str idx ('>':xs) = shorten False ('>':str) idx    xs
    shorten True  str idx (x:xs)   = shorten True  (x:str)   idx    xs
    shorten False str idx (x:xs)
      | idx < maxLen               = shorten False (x:str)  (idx+1) xs
      | otherwise                  = shorten False str      (idx+1) xs
    shorten isTag str idx [] 
      | idx >= maxLen              = reverse $ ".." ++ str
      | otherwise                  = reverse str
