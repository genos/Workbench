blowup :: String -> String
blowup = concatMap (uncurry replicate) . zip [1..]