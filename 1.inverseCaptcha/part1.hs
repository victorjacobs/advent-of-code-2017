import Data.Char
import Data.List.Extra

main :: IO ()
main = do
    content <- readFile "input.txt"
    let trimmedContent = trim content
    let extendedContent = trimmedContent ++ [head trimmedContent]
    print $ inverseCaptcha extendedContent 0

inverseCaptcha :: String -> Int -> Int
inverseCaptcha str@(a : b : _) acc
  | a == b = inverseCaptcha (tail str) (acc + digitToInt a)
  | otherwise = inverseCaptcha (tail str) acc
inverseCaptcha [_] acc = acc
inverseCaptcha [] acc = acc
