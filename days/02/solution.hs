import System.Environment (getArgs)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)

type Position = Int
type Code = Int
type Intcode = Map.Map Position Code

parseInput :: String -> Intcode
parseInput = Map.fromList . zip [0..] . map (read :: String -> Int) . words . map (\char -> if char == ',' then ' ' else char)

runIntcode' :: Intcode -> Position -> Intcode
runIntcode' intcode opcodePosition =
    let {
        firstElPosition = intcode Map.! (opcodePosition + 1);
        maybeFirstEl = Map.lookup firstElPosition intcode;
        secondElPosition = intcode Map.! (opcodePosition + 2);
        maybeSecondEl = Map.lookup secondElPosition intcode;
        intcodeToBreak = Map.fromList [(0, -1)];
    } in if isNothing maybeFirstEl || isNothing maybeSecondEl
        then intcodeToBreak
        else
            let {
                opcode = intcode Map.! opcodePosition;
                (Just firstEl) = maybeFirstEl;
                (Just secondEl) = maybeSecondEl;
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

-- what pair of inputs produces the output 19690720
getInputsWhichProduces19690720' :: Intcode -> Code -> Code -> (Code, Code)
getInputsWhichProduces19690720' intcode noun verb =
    let {
        intcodeCopy = Map.map id intcode;
        haltStateIntcode = runIntcode . Map.insert 2 verb $ Map.insert 1 noun intcodeCopy;
        firstCode = haltStateIntcode Map.! 0;
    } in if firstCode == 19690720
        then (noun, verb)
        else
            if noun == 100
            then (-10, -10)
            else
                if verb == 99
                then getInputsWhichProduces19690720' intcode (noun + 1) 0
                else getInputsWhichProduces19690720' intcode noun (verb + 1)
    
getInputsWhichProduces19690720 :: Intcode -> (Int, Int)
getInputsWhichProduces19690720 intcode = getInputsWhichProduces19690720' intcode 0 0

getSecondPuzzleAnswer :: (Code, Code) -> Int
getSecondPuzzleAnswer (noun, verb) = 100 * noun + verb

--tost :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
--tost noun verb acc = 
--    if noun == 11
--        then acc
--        else 
--            if verb == 6
--            then tost (noun + 1) 0 (acc ++ [(noun, verb)])
--            else tost noun (verb + 1) (acc ++ [(noun, verb)])

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
                --secondPuzzlePart = show $ tost 0 0 [];
                firstPuzzlePart = show $ restore1202State intcode Map.! 0;
                secondPuzzlePart = show . getSecondPuzzleAnswer $ getInputsWhichProduces19690720 intcode;
            } in "First part solution is: " ++ firstPuzzlePart
                ++ "\n" ++ "Second part solution is: " ++ secondPuzzlePart
