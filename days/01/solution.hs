import System.Environment (getArgs)
import qualified Data.List as List

type Mass = Int
type Fuel = Int

parseInput :: String -> [Mass]
parseInput = map read . lines

fuelRequired :: Mass -> Fuel
fuelRequired mass = floor (realToFrac mass / 3) - 2

getRequiredFuelForMassess :: [Mass] -> Fuel
getRequiredFuelForMassess = List.foldl' (\acc x -> acc + fuelRequired x) 0 

totalFuelRequired' :: Mass -> Fuel -> Fuel
totalFuelRequired' mass accFuel =
    let {
        computedFuel = floor (realToFrac mass / 3) - 2;
    } in if computedFuel > 0
        then totalFuelRequired' computedFuel accFuel + computedFuel
        else accFuel

totalFuelRequired :: Mass -> Fuel
totalFuelRequired mass =
    let {
        accFuelInit = 0;
    } in totalFuelRequired' mass accFuelInit

getTotalRequiredFuelForMassess :: [Mass] -> Fuel
getTotalRequiredFuelForMassess = List.foldl' (\acc x -> acc + totalFuelRequired x) 0 

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
                modulesMassList = parseInput input;
                firstPuzzlePart = show $ getRequiredFuelForMassess modulesMassList;
                secondPuzzlePart = show $ getTotalRequiredFuelForMassess modulesMassList;
            } in "First part solution is: " ++ firstPuzzlePart
               ++ "\n" ++ "Second part solution is: " ++ secondPuzzlePart
