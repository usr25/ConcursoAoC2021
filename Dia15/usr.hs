import qualified Data.Map as M
import Data.Char (digitToInt)
import Data.List (minimumBy)

fj (Just a) = a

genCave i (x:xs) = (zip (zip (repeat i) [0..]) x) ++ genCave (i+1) xs
genCave _ [] = []

neighbourDir = [(1,0),(0,1),(-1,0),(0,-1)]
neighbours (x,y) = map (\(dx,dy) ->(x+dx,y+dy)) neighbourDir

expand ls = map (map (+1)) expandedVertically
  where
    expandedHorizontally = map (concat . (\x -> zipWith (\x l -> map (flip rem 9.(+x)) l) [0..4] (repeat x))) ls
    expandedVertically = concat $ zipWith (\x ll -> map (map (flip rem 9.(+x))) ll) [0..4] (repeat expandedHorizontally)

shortestPath m cost final = if M.null cost then final
  else shortestPath m' cost' final'
  where
    (minP,minCost) = minimumBy (\(_,x) (_,y) -> compare x y) (M.assocs cost)
    neighs = filter ((/=Nothing).snd) $ map (\x -> (x,x `M.lookup` m)) $ neighbours minP
    m' = minP `M.delete` m
    cost' = M.delete minP $ foldr aux cost neighs
    final' = M.insert minP minCost final
    
    aux (p,Just v) acc = M.insertWith min p (minCost+v) acc


star m = snd $ M.findMax $ shortestPath m (M.singleton (0,0) 0) M.empty

main :: IO ()
main = do
  contents <- getContents
  let cave = map (map digitToInt) $ lines contents
  let caveM = M.fromList $ genCave 0 cave
  let caveEx = M.fromList $ genCave 0 $ expand $ map (map ((+(-1)))) cave

  putStrLn $ "Star 1: " ++ (show $ star caveM)
  putStrLn $ "Star 2: " ++ (show $ star caveEx)
