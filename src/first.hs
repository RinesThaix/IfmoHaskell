-- Подсчет непустых строк (с учетом пробельных символов и символов перевода строк)

import Data.Char

tinky   ::  Int -> [String] -> Int
isEmpty ::  String -> Int

tinky   total l
    | length l == 0     = total
    | otherwise         = total + (tinky (isEmpty (head l)) (drop 1 l))

isEmpty s
    | length s == 0     = 0
    | isSpace (head s)  = isEmpty (drop 1 s)
    | otherwise         = 1

main = do
    let test = ["Hello", "", "Who art thou", "", "  ", "Hi", "\n\n\n", "2\n "]
    print $ tinky 0 test
    print $ sum (map (\s -> isEmpty s) test)