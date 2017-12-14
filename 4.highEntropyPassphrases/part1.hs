import Data.Char
import Data.List.Extra

main :: IO ()
main = do
    content <- readFile "input.txt"
    print $ length $ filter not $ map (hasDuplicates [] . splitOn " ") (lines $ trim content)

hasDuplicates :: [String] -> [String] -> Bool
hasDuplicates _ [] = False
hasDuplicates ws (x:xs)
  | elem x ws = True
  | otherwise = hasDuplicates (x:ws) xs
