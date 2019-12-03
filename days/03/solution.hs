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

findIntersections :: Path -> Path -> [(Position, Position)]
findIntersections p p1 = Map.keys $ Map.intersection p p1

getNearestPointToCentralPort :: [(Position, Position)] -> (Position, Position)
getNearestPointToCentralPort = head . List.sortOn (\(x, y) -> abs x + abs y)

getDistance :: (Position, Position) -> Int
getDistance (x, y) = abs x + abs y

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
                firstPuzzlePart = show . getDistance . getNearestPointToCentralPort $ findIntersections p p1;
                --secondPuzzlePart = show .  $ ;
            } in "First part solution is: " ++ firstPuzzlePart
                -- ++ "\n" ++ "Second part solution is: " ++ secondPuzzlePart
