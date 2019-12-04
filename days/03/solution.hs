import Prelude hiding (Left, Right, length)
import System.Environment (getArgs)
import qualified Data.Map.Strict as Map
import qualified Data.List as List
--import Data.Maybe (isNothing)

data Direction = Up | Right | Down | Left deriving Show
type Length = Int
data PathSection = Section Direction Length deriving Show

type Position = Int
type X = Position
type Y = Position
type Point = (Position, Position)
type HasWireHere = Bool
type Path = Map.Map (Position, Position) HasWireHere

parseDirection :: Char -> Direction
parseDirection 'U' = Up
parseDirection 'R' = Right
parseDirection 'D' = Down
parseDirection _ = Left

parsePathSection :: String -> PathSection
parsePathSection (directionChar:lengthString) =
    let {
        direction = parseDirection directionChar;
        length = (read :: String -> Int) lengthString;
    } in Section direction length
parsePathSection _ = Section Up (-1)

parseInput :: String -> [[PathSection]]
parseInput = map (map parsePathSection . words . map (\char -> if char == ',' then ' ' else char)) . lines


nextXYInDirection :: X -> Y -> Direction -> (X, Y)
nextXYInDirection x y Up    = (x, y + 1)
nextXYInDirection x y Right = (x + 1, y)
nextXYInDirection x y Down  = (x, y - 1)
nextXYInDirection x y Left  = (x - 1 , y)

data PathWithXY = WithXY Path X Y deriving Show

addSectionToPath :: PathWithXY -> PathSection -> PathWithXY
addSectionToPath pathWithXY (Section _ 0) = pathWithXY
addSectionToPath (WithXY path x y) (Section direction length) =
    let {
        nextCoords@(nX, nY) = nextXYInDirection x y direction;
        nextPath = Map.insert nextCoords True path;
    } in addSectionToPath (WithXY nextPath nX nY) (Section direction (length - 1))

makePath :: [PathSection] -> Path
makePath pathSections =
    let {
        (WithXY path _ _) =
            List.foldl' addSectionToPath (WithXY Map.empty 0 0) pathSections;
    } in path

findIntersections :: Path -> Path -> [Point]
findIntersections p p1 = Map.keys $ Map.intersection p p1

getNearestPointToCentralPort :: [Point] -> Point
getNearestPointToCentralPort = head . List.sortOn (\(x, y) -> abs x + abs y)

type Distance = Int

getDistance :: (Position, Position) -> Distance
getDistance (x, y) = abs x + abs y


type PathDistance = Map.Map (Position, Position) Distance

data PathDistanceWithXYAndIntersections = DistanceWithXYAndIntersections PathDistance X Y [Point] deriving Show


arePairsEqual :: Point -> Point -> Bool
arePairsEqual (x, y) (x1, y1) = x == x1 && y == y1

addSectionToPathDistance :: PathDistanceWithXYAndIntersections -> PathSection -> PathDistanceWithXYAndIntersections
addSectionToPathDistance pathDistanceWithXYAndIntersections (Section _ 0) = pathDistanceWithXYAndIntersections
addSectionToPathDistance pathDistanceWithXYAndIntersections@(DistanceWithXYAndIntersections _ _ _ []) _ =
    pathDistanceWithXYAndIntersections
addSectionToPathDistance (DistanceWithXYAndIntersections pathDistance x y pointsToReach) (Section direction length) =
    let {
        (nX, nY) = nextXYInDirection x y direction;
        nextPathDistance =
            List.foldl' (\ pathDistance' point -> Map.insertWith (+) point 1 pathDistance') pathDistance pointsToReach;
        nextPointsToReach = filter (not . arePairsEqual (nX, nY)) pointsToReach;
    } in addSectionToPathDistance (DistanceWithXYAndIntersections nextPathDistance nX nY nextPointsToReach) (Section direction (length - 1))

getPathDistanceToPoints :: [Point] -> [PathSection] -> PathDistance
getPathDistanceToPoints intersections pathSections =
    let {
        --initialPathDistance = List.foldl' (\ pathDistance point -> Map.insert point 0 pathDistance) Map.empty points;
        initialDistanceWithXY = DistanceWithXYAndIntersections Map.empty 0 0 intersections;
        (DistanceWithXYAndIntersections pathDistance _ _ _) =
            List.foldl' addSectionToPathDistance initialDistanceWithXY pathSections;
    } in pathDistance

getShortestPathDistance :: PathDistance -> Distance
getShortestPathDistance = snd . head . List.sortOn snd . Map.toList

interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith f inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile $ f input

main :: IO ()
main = mainWith solvePuzzle
    where mainWith f = do
            args <- getArgs
            case args of
                [input, output] -> interactWith f input output
                _ -> putStrLn "error: exactly two arguments needed"

          solvePuzzle input =
            let {
                pathSections = parseInput input;
                --firstPuzzlePart = show pathSections;
                [p, p1] = map makePath pathSections;
                intersections = findIntersections p p1;
                firstPuzzlePart = show . getDistance $ getNearestPointToCentralPort intersections;
                --secondPuzzlePart = show $ map (List.sortOn snd . Map.toList . getPathDistanceToPoints intersections) pathSections;
                secondPuzzlePart = show . sum $
                    map (getShortestPathDistance . getPathDistanceToPoints intersections) pathSections;
            } in --"First part solution is: " ++ firstPuzzlePart
                 -- ++ "\n" ++ "Second part solution is: " ++ secondPuzzlePart
                 "Second part solution is: " ++ secondPuzzlePart
