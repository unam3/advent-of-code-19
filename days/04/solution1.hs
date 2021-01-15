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
    [rs1, rs2, rs3, rs4, rs5, rs6] = getMinValidPassword rangeStart;
    [re1, re2, re3, re4, re5, re6] = map digitToInt rangeEnd :: [Int];
} in map show $ [
    [i, j, k, l, m, n] |
        i <- [rs1..9],
        j <- [rs2..9],
        k <- [rs3..9],
        l <- [rs4..9],
        m <- [rs5..9],
        n <- [rs6..9],
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
        [i, j, k, l, m, n] < [re1, re2, re3, re4, re5, re6]
    ]
generatePasswords _ = []

solveTest :: IO ()
solveTest = readFile "textInput"
    >>= print
        . length
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

