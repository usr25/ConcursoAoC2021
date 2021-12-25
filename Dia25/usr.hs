import qualified Data.Map as M

genMap i (x:xs) = (zip (zip (repeat i) [0..]) x) ++ genMap (i+1) xs
genMap _ [] = []

nextMove (_,m) (x,y) '>' = (x, (y+1) `rem` (m+1))
nextMove (n,_) (x,y) 'v' = ((x+1) `rem` (n+1), y)
nextMove a b c = error $ show c

move dims flock m = M.fromList $ map aux cucumbers
  where
    cucumbers = M.assocs flock
    aux (pos,val) = case pos' `M.lookup` m of
            Just _ -> (pos,val)
            Nothing -> (pos',val)
      where
        pos' = nextMove dims pos val

update dims m = M.union east' south'
  where
    east' = move dims east m
    south' = move dims south (M.union east' south)
    south = M.filter (=='v') m
    east = M.filter (=='>') m

star dims prev m i = if prev == m
  then i
  else star dims m (update dims m) (i+1)

axx dims prev m 0 = m
axx dims prev m i = axx dims m (update dims m) (i-1)

main :: IO ()
main = do
  contents <- getContents
  let cave = M.fromList $ genMap 0 $ lines contents
  let (dims,_) = M.findMax cave
  let cave' = M.filter (/='.') cave
  putStrLn $ "Star 1: " ++ (show $ star dims M.empty cave' 0)
  putStrLn $ "Star 2: " ++ ("Done")
