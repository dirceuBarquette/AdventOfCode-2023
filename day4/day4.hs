import Data.Char
import Data.List
import Data.List.Split

main = do
   --file <- readFile "day4Input.txt"
   file <- readFile "input"
   let lns = lines file
       pp2 = parseP2 lns
   putStrLn $ show $ sum $ parse lns
   putStrLn $ show $ sum $ map (snd . snd) $ accInstances pp2

accInstances :: [(Int,(Int,Int))] -> [(Int,(Int,Int))]
accInstances xs = go [] (length xs) 0 xs
   where go acc 0 _ _   = acc
         go acc n t (card@(c,(w,i)):xs)
            | w > 0     = go (acc++[card]) (n-1) 0 $ (spread i (take w xs) ++ (drop w xs))
            | otherwise = go (acc++[card]) (n-1) 0 xs
         spread inst = go' []
            where go' acc' []                = acc'
                  go' acc' ((c',(w',i')):ys) =
                        go' (acc'++[(c',(w',inst+i'))]) ys

parseP2 = parse8 . parse3 . parse2 . parse1

parse8 = map (\(card,[win,deck])-> (read ((words card)!!1)::Int,(length $ intersect win deck,1::Int)))

parse = parse7 . parse6 . parse5 . parse4 . parse3 . parse2 . parse1 
parse7 = map snd
parse6 = map (\(card,wcards)-> (card,2^((length (wcards))-1)))
parse5 = filter (\(_,wcards)-> not $ null wcards)
parse4 = map (\(card,[win,deck])-> (card,intersect win deck))
parse3 = map (\(card,windeck)-> (card,map words windeck))
parse2 = map (\[card,l]-> (card,splitWhen (=='|') l))
parse1 = map (splitWhen (==':'))
