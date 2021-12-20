import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (splitOn, pack, unpack)

neighCoord = [(x,y) | x <- [-1..1], y <- [-1..1]]
neighbours (x,y) = map (\(dx,dy) -> (x+dx,y+dy)) neighCoord

parse input = (V.fromList $ map (fromEnum . (=='#')) $ unpack algorithm,insertions')
  where
    [algorithm,insertions] = splitOn (pack "\n\n") (pack input)
    insertions' = map (map (fromEnum . (=='#'))) $ lines $ unpack insertions

toM ls = M.fromList $ concat $ map (\(x,l) -> map (\(y,e) -> ((x,y),e)) l) $ zip [0..] $ map (zip [0..]) ls

toBin = foldl (\acc x -> 2*acc + x) 0

newVal step m algorithm point = algorithm V.! (coord point)
  where
    coord = toBin . map (\k -> M.findWithDefault step k m) . neighbours

update algorithm step m = M.fromList $ map (\x -> (x,f x)) possible
  where
    f = newVal step m algorithm
    possible = S.toList $ S.fromList $ concat $ map neighbours $ M.keys m

apply n f m = foldl (\acc x -> f (x `mod` 2) acc) m [0..n-1]

star n algorithm input = M.size $ M.filter (==1) $ apply n (update algorithm) (toM input) 

main :: IO ()
main = do
  contents <- getContents
  let (algorithm,input) = parse contents
  putStrLn $ "Star 1: " ++ (show $ star 2 algorithm input)
  putStrLn $ "Star 2: " ++ (show $ star 50 algorithm input)
