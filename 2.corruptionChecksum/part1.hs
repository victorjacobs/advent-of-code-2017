import Data.Char
import Data.List.Extra

main :: IO ()
main = do
    content <- readFile "input.txt"
    print $ splitOn " " $ lines content
