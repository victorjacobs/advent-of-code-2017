import Data.Char
import Data.List.Extra
import Data.Sequence
import Data.Maybe
import Data.Foldable

type Memory = Seq Int
type Position = Int
type State = (Memory, Position)

main :: IO ()
main = do
    content <- readFile "input.txt"
    print $ stepsUntilLoop [fromList $ map read $ splitOn "\t" $ trim content] 0

stepsUntilLoop :: [Memory] -> Int -> Int
stepsUntilLoop history@(x:xs) steps
  | historyContains history newStep = newSteps
  | otherwise = stepsUntilLoop (newStep:history) newSteps
    where newStep = step x
          newSteps = steps + 1

historyContains :: [Memory] -> Memory -> Bool
historyContains [] _ = False
historyContains (x:xs) el
  | x == el = True
  | otherwise = historyContains xs el

step :: Memory -> Memory
step memory = newMemory
  where (maxPosition, amountToDistribute) = findMaxBankFromMemory memory
        memoryWithMaxRemoved = update maxPosition 0 memory
        memoryLength = Data.Sequence.length memory
        (newMemory, _) = distribute (memoryWithMaxRemoved, (maxPosition + 1) `mod` memoryLength) amountToDistribute

findMaxBankFromMemory :: Memory -> (Position, Int)
findMaxBankFromMemory memory = findMaxBank (toList memory) 0 (Nothing, Nothing)

findMaxBank :: [Int] -> Int -> (Maybe Position, Maybe Int) -> (Position, Int)
findMaxBank [] _ (Just currentMaxPosition, Just currentMax) = (currentMaxPosition, currentMax)
findMaxBank (x:xs) currentPosition (Nothing, Nothing) = findMaxBank xs (currentPosition + 1) (Just 0, Just x)
findMaxBank (x:xs) currentPosition currentMaxTuple@(Just currentMaxPosition, Just currentMax)
  | x == currentMax = findMaxBank xs nextPosition currentMaxTuple
  | max x currentMax == x = findMaxBank xs nextPosition (Just currentPosition, Just x)
  | otherwise = findMaxBank xs nextPosition currentMaxTuple
    where nextPosition = currentPosition + 1

distribute :: State -> Int -> State
distribute state 0 = state
distribute (memory, position) amount = distribute (newMemory, newPosition) (amount - 1)
  where memoryLength = Data.Sequence.length memory
        newPosition = (position + 1) `mod` memoryLength
        bank = index memory position
        newMemory = update position (bank + 1) memory
