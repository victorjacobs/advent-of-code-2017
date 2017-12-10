import Data.Char
import Data.List.Extra
import Data.Maybe

type MinMax = (Maybe Int, Maybe Int)

main :: IO ()
main = do
    content <- readFile "input.txt"
    print $ checkSum $ lines $ trim content

checkSum :: [String] -> Int
checkSum lines = foldl (+) 0 $ map checkSumLine lines

checkSumLine :: String -> Int
checkSumLine line = max - min
  where (Just min, Just max) = foldl minMax (Nothing, Nothing) $ map read $ splitOn "\t" line

minMax :: MinMax -> Int -> MinMax
minMax (Nothing, Nothing) num = (Just num, Just num)
minMax (Just oldMin, Just oldMax) num = (Just newMin, Just newMax)
  where newMin = min num oldMin
        newMax = max num oldMax
