-- Шандуренко Константин A3200

-- Task:
-- Make a method maxNumber :: String -> Int, which goal is to determine if there is any number
-- in given string and, whether it is, to get greatest of them.

-- maxNumber - is the required method.
-- maxNumber' - method that transforms string into list of number-strings. It's arguments are:
--      given string, output string-list (which initial value is []), number-string that is currently
--      under construction (digits followed by each other without any other characters between them),
--      current position, length of the given string (because of (length list) complexity being O(n)).
-- maxNumber'' - method that translates list of number-strings into list of numbers & retrieves maximum of them
--      (or throws error, whether given list is empty, so there are no numbers in the initially given string).

import Data.Char

maxNumber       ::  String -> Int
maxNumber'      ::  String -> [String] -> String -> Int -> Int -> [String]
maxNumber''     :: [String] -> Int

maxNumber s                         = maxNumber'' $ maxNumber' s [] [] 0 (length s)

maxNumber' input output current index len
    | index == len                  = if null current then output else output ++ [current]
    | isDigit $ input !! index      = maxNumber' input output (current ++ [input !! index]) (index + 1) len
    | null current                  = maxNumber' input output [] (index + 1) len
    | otherwise                     = maxNumber' input (output ++ [current]) [] (index + 1) len

maxNumber'' s
    | null s                        = error "There are no numbers in given string!"
    | otherwise                     = maximum $ map read s

main = print $
    [
        maxNumber "0xFF55 00012 -100 19"            == 100,
        maxNumber "1"                               == 1,
        maxNumber "100500 -200\n\n\r hell123456ooo" == 123456,
        maxNumber "-000 2"                          == 2,
        maxNumber "0.0,000"                         == 0,
        maxNumber "HELLO WORLD"                     == 0 -- Error case
    ]