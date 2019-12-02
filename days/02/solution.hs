import System.Environment (getArgs)
import qualified Data.Map.Strict as Map

type Position = Int
type Code = Int
type Intcode = Map.Map Position Code

parseInput :: String -> Intcode
parseInput = Map.fromList . zip [0..] . map (read :: String -> Int) . words . map (\char -> if char == ',' then ' ' else char)

runIntcode' :: Intcode -> Position -> Intcode
runIntcode' intcode opcodePosition =
    let {
        opcode = intcode Map.! opcodePosition;
        firstEl = (intcode Map.!) $ intcode Map.! (opcodePosition + 1);
        secondEl = (intcode Map.!) $ intcode Map.! (opcodePosition + 2);
        insertionPosition = intcode Map.! (opcodePosition + 3);
        sumIntcode = Map.insert insertionPosition (firstEl + secondEl) intcode;
        multiplicationIntcode = Map.insert insertionPosition (firstEl * secondEl) intcode;
        newOpcodePosition = opcodePosition + 4;
    } in case opcode of
        1 -> runIntcode' sumIntcode newOpcodePosition
        2 -> runIntcode' multiplicationIntcode newOpcodePosition
        99 -> intcode
        _ -> undefined

runIntcode :: Intcode -> Intcode
runIntcode intcode =
    let {
        opcodeInitPosition = 0;
    } in runIntcode' intcode opcodeInitPosition

restore1202State :: Intcode -> Intcode
restore1202State = runIntcode . Map.insert 2 2 . Map.insert 1 12


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
                intcode = parseInput input;
                --firstPuzzlePart = show $ runIntcode intcode;
                firstPuzzlePart = show $ restore1202State intcode Map.! 0;
                --secondPuzzlePart = show $ getTotalRequiredFuelForMassess modulesMassList;
            } in "First part solution is: " ++ firstPuzzlePart
               -- ++ "\n" ++ "Second part solution is: " ++ secondPuzzlePart
