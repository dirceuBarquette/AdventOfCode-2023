import Data.List.Split
import Data.List

main = do
   file <- readFile "input"
   let lns = lines file
   let games= map evalGame lns
   let possibles = map (\[r,g,b]-> r*g*b) games
   putStrLn $ show $ sum possibles

evalGame :: String -> [Int]
evalGame str = result
   where
      [identS,grabsS] = splitWhen (==':') str
      i = read ((words identS) !! 1) :: Int
      grabs = splitWhen (==';') grabsS
      grab = map (splitWhen (==',')) grabs
      colorsInEachGrab = map (\l-> map words l) grab
      totalPerGrab = map (\unw-> foldl (\acc qAndc->
                           getRGB acc qAndc) [0,0,0] unw) colorsInEachGrab
      result = map maximum $ transpose totalPerGrab

getRGB :: [Int] -> [String] -> [Int]    
getRGB [r,g,b] [qtt,color] =    
   case color of    
      "red"   -> [qInt,g,b]   
      "green" -> [r,qInt,b]
      "blue"  -> [r,g,qInt]    
   where qInt = read qtt::Int
