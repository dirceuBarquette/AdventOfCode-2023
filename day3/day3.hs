import Data.Char
import Data.List

instance Semigroup Coord where
   (Coord x y Numb v) <> (Coord _ _ Numb v') = (Coord x y Numb (v++v'))
   (Coord x y Numb v) <> (Coord _ _ _ _)     = (Coord x y Numb v)
   (Coord x y e v)    <> (Coord _ _ _ _)     = (Coord x y e v)

data ElType = Numb | Symb | DOt deriving (Show,Eq)
data Coord = Coord { x :: Int
                   , y :: Int
                   , elType :: ElType
                   , value :: String
                   } deriving (Show,Eq)

main = do
   --file <- readFile "day3Input.txt"
   file <- readFile "input"
   let lns = lines file
       coords = mapCoord lns
       rem = remap coords
       nums = filter ((== Numb) . elType ) rem
       symbs = filter ((== Symb) . elType) rem
       valids = filter (\num-> isNumValid num symbs) nums
       gears = filter ((== "*") . value) symbs 
       valNG = filter (\num-> isNumValid num gears) valids
       valids' = getNumsP2 $ map (\num-> (read (value num)::Int, inv num gears)) valNG
   putStrLn $ show $ sum $ map ((\v->read v::Int)  . value ) valids
   putStrLn $ show $ sum $ map (\[(n1,_),(n2,_)]-> n1*n2) valids'

getPossibilities :: Coord -> [(Int,Int)]
getPossibilities (Coord x y _ v) =
   let aboveX = take (2 + length v) $ repeat (x-1)
       belowX = take (2 + length v) $ repeat (x+1)
       coordY = [(y-1)..(y + (length v))]
       nextXY = [(x,y-1),(x,y + length v)]
       coord1 = zip aboveX coordY
       coord2 = zip belowX coordY
   in  concat [nextXY, coord1,coord2]

getNumsP2 :: [(Int,(Int,Int))] -> [[(Int,(Int,Int))]]
getNumsP2 = go []
   where
      go acc [] = filter ((== 2) . length) acc
      go acc l@(x:_) =
         let (true,false) = partition (\(_,coord)-> (snd x)==coord) l
         in go ([true]++acc) false

inv :: Coord -> [Coord] -> (Int, Int)
inv coord symbs = c
   where
      possib = getPossibilities coord
      gearCoords = map (\(Coord x y _ _)-> ((x,y),(elemIndices (x,y) possib))) symbs
      gear = filter (not . null . snd) 
      [c] = map fst $ gear gearCoords

isNumValid :: Coord -> [Coord] -> Bool
isNumValid coord symbs =
   let possib = getPossibilities coord
   in or $ map (\(Coord x y _ _)-> (x,y) `elem` possib) symbs

mapCoord :: [String] -> [Coord]
mapCoord str = 
   let zipX = zip [0..] str
       el s | isDigit s = Numb
            | s == '.'  = DOt
            | otherwise = Symb
   in [Coord x y (el v) [v] |(x,l) <- zipX, (y,v) <- zip [0..] l ]
      
remap :: [Coord] -> [Coord]
remap coords =
   let fn (num,ac) c'
         | elType c' /= Numb = 
            if null num then ([],ac++[c']) else ([],ac++num++[c'])
         | otherwise =
            if null num then ([c'],ac) else ([head num <> c'],ac) 
   in snd $ foldl (\acc c-> fn acc c) ([],[]) coords
