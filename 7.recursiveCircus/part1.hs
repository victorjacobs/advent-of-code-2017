import Data.Char
import Data.List.Extra
import Data.List
-- import qualified Data.Map.Strict as Map

type Node = (String, [String])

main :: IO ()
main = do
    content <- readFile "input.txt"
    print $ findRoot $ mergeNodeDescriptions (map parseLineWithChildren $ filter lineHasChildren $ lines content) ([], [])

lineHasChildren :: String -> Bool
lineHasChildren = isInfixOf "->"

parseLineWithChildren :: String -> Node
parseLineWithChildren str = (nodeName, children)
  where nodeName = head $ splitOn " " str
        childrenString = last $ splitOn " -> " str
        children = splitOn ", " childrenString

mergeNodeDescriptions :: [Node] -> ([String], [String]) -> ([String], [String])
mergeNodeDescriptions [] merged = merged
mergeNodeDescriptions ((nodeName, children):xs) (currentParents, currentChildren) =
  mergeNodeDescriptions xs (nodeName:currentParents, children ++ currentChildren)

findRoot :: ([String], [String]) -> String
findRoot (parent:parents, children)
  | not (any (hasParent parent) [children]) = parent
  | otherwise = findRoot (parents, children)

hasParent :: String -> [String] -> Bool
hasParent _ [] = False
hasParent nodeName (x:xs)
  | nodeName == x = True
  | otherwise = hasParent nodeName xs
