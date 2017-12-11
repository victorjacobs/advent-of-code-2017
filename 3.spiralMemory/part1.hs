import Prelude hiding (Left, Right)

import Data.List.Extra
import qualified Data.Map.Strict as Map

type Coordinate = (Int, Int)
type VisitedSquares = Map.Map Coordinate Bool
type State = (VisitedSquares, Coordinate, Orientation)
data Orientation = Right | Up | Left | Down deriving (Show)

main :: IO ()
main = print $ distance 265149

distance :: Int -> Int
distance address = distanceFromState $ step (Map.fromList[((0, 0), True), ((1, 0), True)], (1, 0), Right) (address - 2)

distanceFromState :: State -> Int
distanceFromState (_, (x, y), _) = abs x + abs y

step :: State -> Int -> State
step state 0 = state
step (visitedSquares, (x, y), orientation) n = step (newVisitedSquares, (newX, newY), newOrientation) (n - 1)
  where newVisitedSquares = visitSquare visitedSquares (x, y)
        ((newX, newY), newOrientation) = move visitedSquares (x, y) orientation

visitSquare :: VisitedSquares -> Coordinate -> VisitedSquares
visitSquare visitedSquares coordinate = Map.insert coordinate True visitedSquares

move :: VisitedSquares -> Coordinate -> Orientation -> (Coordinate, Orientation)
move visitedSquares coordinate orientation
  | didVisitLeft = (moveInOrientation coordinate orientation, orientation)
  | not didVisitLeft = (moveInOrientation coordinate newOrientation, newOrientation)
    where didVisitLeft = Map.findWithDefault False (coordinateLeft coordinate orientation) visitedSquares
          newOrientation = turnLeft orientation

moveInOrientation :: Coordinate -> Orientation -> Coordinate
moveInOrientation (x, y) Right = (x + 1, y)
moveInOrientation (x, y) Up = (x, y + 1)
moveInOrientation (x, y) Left = (x - 1, y)
moveInOrientation (x, y) Down = (x, y - 1)

turnLeft :: Orientation -> Orientation
turnLeft Right = Up
turnLeft Up = Left
turnLeft Left = Down
turnLeft Down = Right

coordinateLeft :: Coordinate -> Orientation -> Coordinate
coordinateLeft (x, y) Right = (x, y + 1)
coordinateLeft (x, y) Up = (x - 1, y)
coordinateLeft (x, y) Left = (x, y - 1)
coordinateLeft (x, y) Down = (x + 1, y)
