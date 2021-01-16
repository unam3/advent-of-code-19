module SecureContainer where

import Data.Char (digitToInt)
import Data.List (group)

type Number = (Char, Char, Char, Char, Char, Char)

mapDashToWhitespace :: Char -> Char
mapDashToWhitespace '-' = ' '
mapDashToWhitespace char = char

parseInput :: String -> [String]
parseInput = words . map mapDashToWhitespace

hasSameAdjacentDigits :: String -> Bool
hasSameAdjacentDigits = (< 6) . length . group

getMinValidPassword :: String -> [Int]
getMinValidPassword = reverse . foldl
    (\ acc char -> let {
        number = digitToInt char;
        prevNumber = head acc;
    } in if null acc
        then number : acc
        else if number <= prevNumber
            then prevNumber : acc
            else number : acc
        )
    []

generatePasswords :: [String] -> [String]
generatePasswords [rangeStart, rangeEnd] = let {
    rangeStartDigits = getMinValidPassword rangeStart;
    rangeEndDigits = map digitToInt rangeEnd :: [Int];
} in map show [
    [i, j, k, l, m, n] |
        i <- [0..9],
        j <- [0..9],
        k <- [0..9],
        l <- [0..9],
        m <- [0..9],
        n <- [0..9],
        -- next digit same or bigger
        j >= i,
        k >= j,
        l >= k,
        m >= l,
        n >= m,
        -- at lest one pair
        j == i ||
        k == j ||
        l == k ||
        m == l ||
        n == m,
        [i, j, k, l, m, n] >= rangeStartDigits,
        [i, j, k, l, m, n] < rangeEndDigits
    ]
generatePasswords _ = []

solveTest :: IO ()
solveTest = readFile "testInput"
    >>= print
        . generatePasswords
        . parseInput
        . init

solve :: IO ()
solve = readFile "input"
    >>= print
        . length
        . generatePasswords
        . parseInput
        . init

main :: IO ()
main = solveTest

