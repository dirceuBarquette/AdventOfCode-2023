import Data.Char

main = do
   file <- readFile "input"
   let lns = lines file
   let nums = map (\w-> filter isDigit w) lns
   let s = map (\w-> read [head w,last w] :: Int) nums
   putStrLn $ show $ foldl (+) 0 s
