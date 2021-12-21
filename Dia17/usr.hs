parse coords = (findTuple x',findTuple y')
  where
    [x'',y'] = map (drop 2) $ drop 2 coords
    x' = init x''
    findTuple s = (read frst,read scnd')
      where
        (frst,scnd) = span (/='.') s
        scnd' = dropWhile (=='.') scnd

vals ((x0,x1),(y0,y1)) = [(x,y) | y <- reverse [y0.. -y0], x <- [1..x1]]

isValid range@((x0,x1),(y0,y1)) (x,y) (dx,dy)
  | x > x1 || y < y0 = False
  | x0 <= x && x <= x1 && y0 <= y && y <= y1 = True
  | otherwise = isValid range (x+dx,y+dy) (max 0 (dx-1), dy-1)

apex y = y*(y+1) `div` 2

star1 range = apex $ snd $ head $ filter (isValid range (0,0)) $ vals range
star2 range = length $ filter (isValid range (0,0)) $ vals range

main :: IO ()
main = do
  contents <- getContents
  let coords = parse $ words contents
  putStrLn $ "Star 1: " ++ (show $ star1 coords)
  putStrLn $ "Star 2: " ++ (show $ star2 coords)
