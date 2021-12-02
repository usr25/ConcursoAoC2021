move1 [hor,depth] (dir,ammount)
  | dir == "forward" = [hor+ammount,depth]
  | dir == "down" = [hor,depth+ammount]
  | dir == "up" = [hor,depth-ammount]

move2 [hor,depth,aim] (dir,ammount)
  | dir == "forward" = [hor+ammount,depth+aim*ammount,aim]
  | dir == "down" = [hor,depth,aim+ammount]
  | dir == "up" = [hor,depth,aim-ammount]

star1 = product . foldl move1 [0,0]
star2 = product . init . foldl move2 [0,0,0]

main :: IO ()
main = do
  contents <- getContents
  let dirs = map ((\[x,y] -> (x,read y :: Int)) . words) $ lines contents
  putStrLn $ "Star 1: " ++ (show $ star1 dirs)
  putStrLn $ "Star 2: " ++ (show $ star2 dirs)
