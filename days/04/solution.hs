import Prelude hiding (Left, Right, length)
import System.Environment (getArgs)
--import qualified Data.Map.Strict as Map
import qualified Data.List as List

type NumberDigits = [Int]

-- https://stackoverflow.com/a/6308107/3484083
-- returns empty list if pass zero
-- ignores leading zeroes
digits :: Int -> NumberDigits
digits = reverse . List.unfoldr (\x -> if x == 0
                                       then Nothing
                                       else Just (mod x 10, div x 10))

--Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
makeVariants :: NumberDigits -> NumberDigits -> [[Int]] -> [[Int]]
makeVariants (firstRangeStartDigit:secondRangeStartDigit:sn) rangeEnd [] =
    let {
        lowerBound = max firstRangeStartDigit secondRangeStartDigit;
        newVariants = [[firstRangeStartDigit], [lowerBound..9]]
    } in makeVariants sn rangeEnd newVariants
    
makeVariants rangeStartDigits@(firstRangeStartDigit:rsd) rangeEndDigits variants =
    let {
        lastLowRangeStartVariantDigit = head $ last variants;
        lowerBound = max lastLowRangeStartVariantDigit firstRangeStartDigit;
        newVariants = variants ++ [[lowerBound..9]];
    } in makeVariants rsd rangeEndDigits newVariants

makeVariants [] _ variants = variants
makeVariants _ _ _ = [[]]
    
getMinPassword :: [[Int]] -> NumberDigits
getMinPassword = undefined


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
                firstPuzzlePart = show . map (digits . read) $ lines input;
                --secondPuzzlePart = show .;
            } in "First part solution is: " ++ firstPuzzlePart
                -- ++ "\n" ++ "Second part solution is: " ++ secondPuzzlePart
