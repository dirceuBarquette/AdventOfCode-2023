import Data.List.Split

main = do
   file <- readFile "input"
   let lns = lines file
   let games= map evalGame lns
   let possibles = fmap sum games
   putStrLn $ show $ sum possibles

evalGame :: String -> Maybe Int
evalGame str = result
   where
      [identS,grabsS] = splitWhen (==':') str
      i = read ((words identS) !! 1) :: Int
      grabs = splitWhen (==';') grabsS
      grab = map (splitWhen (==',')) grabs
      colorsInEachGrab = map (\l-> map words l) grab
      isPossible = and $ map (\unw-> and $ map (\qAndc->
                          compRGB qAndc) unw) colorsInEachGrab
      result = if isPossible then Just i else Nothing

compRGB :: [String] -> Bool
compRGB [qtt,color] =
   case color of
      "red"   -> qInt <= 12
      "green" -> qInt <= 13
      "blue"  -> qInt <= 14
   where qInt = read qtt::Int
