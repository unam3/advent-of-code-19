import System.Environment (getArgs)
import qualified Data.List as List

type Mass = Int
type Fuel = Int

parseInput :: String -> [Mass]
parseInput = map read . lines

fuelRequired :: Mass -> Fuel
fuelRequired x = floor (realToFrac x / 3) - 2

getRequiredFuelForMassess :: [Mass] -> Fuel
getRequiredFuelForMassess = List.foldl' (\acc x -> acc + fuelRequired x) 0 

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
              "First part solution is: " ++ firstPuzzlePart where
                firstPuzzlePart = let {
                        modulesMassList = parseInput input;
                    } in show $ getRequiredFuelForMassess modulesMassList
