-- Шандуренко Константин A3200

-- Task:
-- Make a method maxDeriv :: Real a => [a] -> Int that returns index of the
-- element with max derivative. In this statement we determine derivative of i-th element in list
-- as a subtraction of (i+1)-th and i-th elements. As for last element of the list, let's say
-- that his derivative is equal to zero.

-- maxDeriv - is the required method.
-- maxDeriv' - the method that generates list of derivatives. It's arguments are
--      the given list of elements, the list of derivatives (which initial value is empty list),
--      length of the first list (in case of 'length list' complexity being O(n)) and the index of element
--      we're currently at.
-- maxDeriv'' - the method that transforms list of reals into index of maximum element in it.
-- maxDeriv''' - the method that does the same thing as the maxDeriv', but in another way.

maxDeriv        ::  Real a => [a] -> Int
maxDeriv'       ::  Real a => [a] -> [a] -> Int -> Int -> [a]
maxDeriv''      ::  Real a => [a] -> Int
maxDeriv'''     ::  Real a => [a] -> [a]

maxDeriv    l
    | null l            = error "List can not be empty!"
--    | otherwise         = maxDeriv'' $ maxDeriv' l [] (length l) 0
    | otherwise         = maxDeriv'' $ maxDeriv''' l

maxDeriv' input output len index
    | index == len      = output
    | index == len - 1  = maxDeriv' input (output ++ [0]) len (index + 1)
    | otherwise         = maxDeriv' input (output ++ [(input !! (index + 1)) - (input !! index)]) len (index + 1)

maxDeriv'' l            = head $ filter ((== maximum l) . (l !!)) [0..]

maxDeriv''' l           = map (\x -> (l !! (x + 1)) - (l !! x)) [0..(length l) - 2] ++ [0]

main = print $
    [
        maxDeriv [3, 1, 2, 5, 7, 6] == 2,
        maxDeriv [1]                == 0,
        maxDeriv [0, 1, 2, 3]       == 0,
        maxDeriv [0, -1, -3, -6]    == 3,
        maxDeriv [8, 2, -4, 16, 5]  == 2
    ]