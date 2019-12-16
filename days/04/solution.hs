import Prelude hiding (Left, Right, length)
import System.Environment (getArgs)
--import qualified Data.Map.Strict as Map
import qualified Data.List as List

type NumberDigits = [Int]
type HasSameAdjacentDigitsPair = Bool
data NumberDigitsAndShit = DigitsAndShit NumberDigits HasSameAdjacentDigitsPair deriving Show
data MinMaxPasswords = MinMax NumberDigitsAndShit NumberDigitsAndShit deriving Show

-- https://stackoverflow.com/a/6308107/3484083
-- returns empty list if pass zero
-- ignores leading zeroes
digits :: Int -> NumberDigits
digits = reverse . List.unfoldr (\x -> if x == 0
                                       then Nothing
                                       else Just (mod x 10, div x 10))

processInput :: String -> [NumberDigits]
processInput str =
    let {
        rangeStartDigits = digits . read $ take 6 str;
        rangeEndDigits = digits . read $ drop 7 str;
    } in [rangeStartDigits, rangeEndDigits]


minMaxNumbersDigits :: [NumberDigits] -> MinMaxPasswords
minMaxNumbersDigits [rangeStartDigits, rangeEndDigits] =
    minMaxNumbersDigits' rangeStartDigits rangeEndDigits (MinMax (DigitsAndShit [] False) (DigitsAndShit [] False))

minMaxNumbersDigits' :: NumberDigits -> NumberDigits -> MinMaxPasswords -> MinMaxPasswords
minMaxNumbersDigits' (firstRangeStartDigit:secondRangeStartDigit:sn) (firstRangeEndDigit:secondRangeEndDigit:en) (MinMax (DigitsAndShit [] False) (DigitsAndShit [] False)) =
    let {
        lowerBound = max firstRangeStartDigit secondRangeStartDigit;
        upperBound = max firstRangeEndDigit secondRangeEndDigit;
        minHasSameAdjacentDigitsPair = firstRangeStartDigit == lowerBound;
        maxHasSameAdjacentDigitsPair = firstRangeEndDigit == upperBound;
        minDigitsAndShit = DigitsAndShit [firstRangeStartDigit, lowerBound] minHasSameAdjacentDigitsPair;
        maxDigitsAndShit = DigitsAndShit [firstRangeEndDigit, upperBound] maxHasSameAdjacentDigitsPair;
        newVariants = MinMax minDigitsAndShit maxDigitsAndShit;
    } in minMaxNumbersDigits' sn en newVariants

minMaxNumbersDigits' (firstRangeStartDigit:rsd) (firstRangeEndDigit:red) (MinMax (DigitsAndShit minNumberDigits minHasSameAdjacentDigitsPair) (DigitsAndShit maxNumberDigits maxHasSameAdjacentDigitsPair)) =
    let {
        lastLowerRangeStartVariantDigit = last minNumberDigits;
        lastUpperRangeEndVariantDigit = last maxNumberDigits;
        lowerBound = max lastLowerRangeStartVariantDigit firstRangeStartDigit;
        upperBound = max lastUpperRangeEndVariantDigit firstRangeEndDigit;
        minHasSameAdjacentDigitsPair' =
            if minHasSameAdjacentDigitsPair
            then True
            else lastLowerRangeStartVariantDigit == lowerBound;
        maxHasSameAdjacentDigitsPair' =
            if maxHasSameAdjacentDigitsPair
            then True
            else lastUpperRangeEndVariantDigit == upperBound;
        digitsListEnd = null rsd;
        newMinNumberDigits =
            if digitsListEnd && not minHasSameAdjacentDigitsPair'
            -- suit only for 6 digits ranges
            then (take 4 minNumberDigits) ++ [lowerBound, lowerBound]
            else minNumberDigits ++ [lowerBound];
        newMaxNumberDigits =
            if digitsListEnd && not maxHasSameAdjacentDigitsPair'
            then let {
                compensatedUBound = upperBound - 1;
            } in (take 4 maxNumberDigits) ++ [compensatedUBound, compensatedUBound]
            else maxNumberDigits ++ [upperBound];
        newMinMax = MinMax (DigitsAndShit newMinNumberDigits minHasSameAdjacentDigitsPair') (DigitsAndShit newMaxNumberDigits maxHasSameAdjacentDigitsPair');
    } in minMaxNumbersDigits' rsd red newMinMax

minMaxNumbersDigits' _ _ minMax = minMax


--Two adjacent digits are the same (like 22 in 122345).
--Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
--countMatchedPasswords :: Int -> Int -> Int
--countMatchedPasswords from to =
--    let {
--        rangeStartDigits = digits from;
--        rangeEndDigits = digits to;
--        variantsWithoutMandatoryPair =
--            fmap (\ lowerBound upperBound -> [lowerBound..upperBound]) $
--            zip rangeStartDigits rangeEndDigits;
--        matchedPasswordsCount = 42;
--    } in matchedPasswordsCount

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
                firstPuzzlePart = show . minMaxNumbersDigits $ processInput input;
                --firstPuzzlePart = show  .
                --    (\ [rangeStartDigits, rangeEndDigits] -> minMaxNumbersDigits rangeStartDigits rangeEndDigits) .
                --    map (digits . read) $ lines input;
                ----secondPuzzlePart = show .;
            } in "First part solution is: " ++ firstPuzzlePart
                -- ++ "\n" ++ "Second part solution is: " ++ secondPuzzlePart
