import Data.Char
import Data.List.Extra
import Data.Sequence

type State = (Seq Int, Int)

main :: IO ()
main = do
    content <- readFile "input.txt"
    print $ stepsUntilOutOfBounds (fromList $ map read $ lines $ trim content, 0)

stepsUntilOutOfBounds :: State -> Int
stepsUntilOutOfBounds initialState@(jumps, _) = position
  where mazeLength = Data.Sequence.length jumps
        position = Prelude.length $ takeWhile ((< mazeLength) . snd) $ iterate step initialState

step :: State -> State
step (jumps, position) = (newJumps, newPosition)
  where currentJump = index jumps position
        newPosition = position + currentJump
        newJumps = update position (currentJump + 1) jumps
