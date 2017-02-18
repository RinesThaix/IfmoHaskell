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

-- Тупая сортировка

insert  :: (Ord a) => a -> [a] -> [a]
insert' :: (Ord a) => a -> [a] -> [a] -> [a]
sort    :: (Ord a) => [a] -> [a]
sort'   :: (Ord a) => [a] -> [a] -> [a]

insert el l             = insert' el [] l

insert' el l1 l2
    | length l2 == 0    = l1 ++ [el]
    | el < head l2      = l1 ++ [el] ++ l2
    | otherwise         = insert' el (l1 ++ [(head l2)]) (drop 1 l2)

sort l                  = sort' [] l

sort' l1 l2
    | length l2 == 0    = l1
    | otherwise         = sort' (insert (head l2) l1) (drop 1 l2)

main = do
    let test = ["Hello", "", "Who art thou", "", "  ", "Hi", "\n\n\n", "2\n "]
    print $ tinky 0 test
    print $ sum (map (\s -> isEmpty s) test)
    print $ sort [1, 2, 16, 13, 8, -5, 16, 22, 19, 64, 0]