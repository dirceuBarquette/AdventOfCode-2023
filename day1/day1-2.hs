import Data.Char
import Data.List

patterns :: [String]
patterns = ["one","two","three","four","five","six","seven","eight","nine"]
snrettap = map reverse patterns 

main = do
   file <- readFile "input"
   let lns = lines file
   let mapl = map (isDigOrPat patterns) lns
   let convl = map convert mapl
   let snl = map reverse lns
   let mapr = map (isDigOrPat snrettap) snl
   let revMapr = map reverse mapr
   let convRev = map convert revMapr
   let zipConvs = zipWith (\x y-> x*10+y) convl convRev 
   putStrLn $ show $ foldl (+) 0 zipConvs

convert :: String -> Int
convert str =
   case elemIndex str patterns of
      Just x  -> x + 1
      Nothing -> read str::Int

isDigOrPat :: [String] -> String -> String
isDigOrPat patt = go ("","") False
   where
      go :: (String,String) -> Bool -> String -> String
      go (_,ret) True _       = ret
      go (acc,_) False (x:xs) =
         if length match == 1
         then go ("",match !! 0) True ""
         else
            if isDigit x
            then go ("",x:[]) True ""
            else go (acc ++ [x],"") False xs
         where match = isinfixof acc patt

isinfixof :: String -> [String] -> [String]
isinfixof needle =
   filter (\p-> p `isInfixOf` needle)
