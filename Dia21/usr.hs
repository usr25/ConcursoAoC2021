import Data.List (partition)
import qualified Data.Map as M

wrapMod x m = if mo == 0 then m else mo
  where mo = x `rem` m

move (die, ((p1,score1),(p2,score2))) = ((die+6) `wrapMod` 100, ((p1Pos',score1+p1Pos'),(p2Pos',score2+p2Pos')))
  where
    sum3 x = (3*x + 1 + 2)
    p1Pos' = (p1+sum3 die) `wrapMod` 10
    p2Pos' = (p2+sum3 (die+3)) `wrapMod` 10

apply n f m = foldl (\acc _ -> f acc) m [1..n]

gameDone (_,((_,s1),(_,s2))) = s1 >= 1000 || s2 >= 1000
genGame f initial = takeWhile (not.gameDone.snd) $ scanl (\(_,acc) n -> (n+1,f acc)) initial [1..]


diceThrowProportion = [(3,1), (4,3), (5,6), (6,7), (7,6), (8,3), (9,1)]

newStates (p,score) = map aux diceThrowProportion
  where
    aux (move,proportion) = (proportion, (p',p'+score))
      where p' = (p+move) `wrapMod` 10

limit = 20

prop (p,_,_) = p
score1 (_,(_,s),_) = s
score2 (_,_,(_,s)) = s

aggregate = map (\((s1,s2),t) -> (t,s1,s2)) . M.assocs . fold M.empty

fold acc [] = acc
fold acc ((t,s1,s2):xs) = fold (M.insertWith (+) (s1,s2) t acc) xs

wins _ [] = (0,0)
wins 0 states = ((sum $ map prop ws) + w1, w2)
  where
    ns = aggregate $ concat $ map (\(p,x,y) -> zipWith (\(t,s1) s2 -> (p*t,s1,s2)) (newStates x) (repeat y)) states
    (ws,notYet) = partition ((>limit).score1) ns
    (w1,w2) = wins 1 notYet
wins 1 states = (w1, (sum $ map prop ws) + w2)
  where
    ns = aggregate $ concat $ map (\(p,x,y) -> zipWith (\s1 (t,s2) -> (p*t,s1,s2)) (repeat x) (newStates y)) states
    (ws,notYet) = partition ((>limit).score2) ns
    (w1,w2) = wins 0 notYet

star1 [p1,p2] = if f1 >= f2
  then s2*(turn*6-3)
  else s1*(turn*6)
  where 
    (_,((_,f1),(_,f2))) = move lastState
    (_,((_,s1),(_,s2))) = lastState
    (turn,lastState) = last $ genGame move (0,(1, ((p1,0),(p2,0))))

star2 [p1,p2] = uncurry max $ wins 0 [(1,(p1,0),(p2,0))]

main :: IO ()
main = do
  contents <- getContents
  let players = map (read . last . words) $ lines contents
  putStrLn $ "Star 1: " ++ (show $ star1 players)
  putStrLn $ "Star 2: " ++ (show $ star2 players)
