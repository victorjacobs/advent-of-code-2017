import Data.Char
import Data.List.Extra hiding (take)
import Data.Sequence hiding (take)

type State = (Seq Int, Int)

main :: IO ()
main = do
    content <- readFile "input.txt"
    print $ stepsUntilOutOfBounds (fromList $ map read $ lines $ trim content, 0)

stepsUntilOutOfBounds :: State -> Int
stepsUntilOutOfBounds initialState@(jumps, _) = position
  where mazeLength = Data.Sequence.length jumps
        position = Prelude.length $ takeWhile (< mazeLength) [positionAfterSteps x initialState | x <- [1 ..]]

positionAfterSteps :: Int -> State -> Int
positionAfterSteps iterations state = position
  where (_, position) = last $ take iterations $ iterate step state

step :: State -> State
step (jumps, position) = (newJumps, newPosition)
  where currentJump = index jumps position
        newPosition = position + currentJump
        newJumps = update position (currentJump + 1) jumps
