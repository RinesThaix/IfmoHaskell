-- Шандуренко Константин A3200

-- Задание:
-- Написать функцию hasPair :: Integer -> Bool, которая проверяет,
-- есть ли в десятичной записи заданного числа две подряд идущие одинаковые цифры.
-- Например, hasPair 1001 => True, а hasPair 1212 => False.

-- hasPair :: Int -> Bool
-- Собственно, требуемая функция. В себе проверяет, меньше ли по модулю передаваемое число десяти, если да, -
-- в нем меньше двух цифр, так что ответ нет, иначе запустимся от функции hasPair' с аргументами num/10 и num%10

-- hasPair' :: Int -> Int -> Bool
-- Принимает два аргумента: число и цифру (второе число), и проверяет,
-- оканчивается ли первое число на переданную цифру.
-- Если да, то мы победили; если нет, то проверим, что наше число по модулю меньше десятки,
-- а дальше запустимся от самих себя, разделив число на 10, а в качестве цифры передав остаток от деления числа на 10

hasPair     ::  Int -> Bool
hasPair'    ::  Int -> Int -> Bool

hasPair     num
    | abs num < 10              = False
    | otherwise                 = hasPair' (quot num 10) (num `mod` 10)


hasPair'    div md
    | (div `mod` 10) == md      = True
    | abs div < 10              = False
    | otherwise                 = hasPair' (quot div 10) (div `mod` 10)


-- Тесты
main = print $
    [
        hasPair 1001        == True,
        hasPair 1212        == False,
        hasPair 2           == False,
        hasPair 0           == False,
        hasPair 11          == True,
        hasPair 37          == False,
        hasPair (negate 9)  == False,
        hasPair (negate 10) == False,
        hasPair (negate 33) == True,
        hasPair 229876      == True,
        hasPair 987677      == True,
        hasPair 00678       == False,
        hasPair 6450062     == True,
        hasPair 2937612300  == True,
        hasPair 1927358606  == False
    ]