-- TASK1

--сортирую список кортежей по количеству символов

sortBy :: [(Char,Int)] -> [(Char,Int)]
sortBy [] = []
sortBy x = mix : sortBy xx
           where mix = foldl (\ a p -> if fst a > fst p then p else a) (head x) x
                 xx  = filter (/= mix) x
                 
-- сравниваю два списка кортежей между собой

compare (x:xs) (y:ys) = (x == y) && compare xs ys
compare [] [] = True
compare _ _ = False

-- считаю количество букв в строке
               
countChars :: String -> [(Char,Int)]
countChars ""     = []
countChars (s:ss) =  (s,n) : countChars ww   
                       where ww = filter (/= s) ss
                             n  = length ss-length ww+1
                             
subset :: [String] -> [[String]]
subset [x] = [[x], []]
subset (x:ls) = current ++ map (x:) current
            where current = subset ls               
 
-- проверяю на соответствие встречаемости символов в исходной строке string для множества слов, выбираю лишь совпадающие
anagrams :: String -> [String] ->[[String]]
anagrams string dict =  filter (isEqual inp_string) subsets     
                        where subsets = subset dict
                              inp_string = sortBy $ countCharsMy (head (cutWhitespace [string]))
                              isEqual s1 s2 = compare s1 (sortBy $ countCharsMy (concat s2)) 
                              cutWhitespace (x:ls) = filter (/=' ') x:ls
                              
-- ПРОВЕРКА: 
-- let input_string = "madonna louise ciccone"
-- let dictionary = ["come","cool","dance","in","income","musician","nude","occasional","one","two"]
-- anagrams input_string dictionary:
-- [["income","nude","occasional"],["cool","dance","musician","one"],["come","in","nude","occasional"]]



-- TASK2

import Data.List
import Data.List.Split


-- вспомогательная функция - разделения для дерева
splits :: Eq a => [a] -> [([a], [a])]
splits xs = [ (as,bs) | (as,bs) <- zip (inits xs) (tails xs), as /= [], bs /= [] ]


-- операции + - и слияние
type Operation = Char
oper :: [Operation]
oper = ['+', '-', '\0'] 



-- Наше дерево выражений, в котором будут храниться значения и операции между ними
data Node = Value Integer  -- лист дерева с некоторым числовым значением 
            | Op Operation Node Node -- узел дерева: операция и два поддерева
            deriving Show

-- генерация всевозможных последовательность из цифр 1..9, +,- и слияния
gen :: [Integer] -> [Node]
gen [] = []
gen [x] = [Value x]
gen xs = [ Op op leftNode rightNode | (leftList,rightList) <- splits xs, 
                 leftNode <- gen leftList, 
                 rightNode <- gen rightList,
                 op <- oper]

-- для визуализации ответа - собираем дерево 
mynode :: Node -> String
mynode (Value a) = show a
mynode (Op op left right) | op == '\0' = mynode left ++ "" ++ mynode right
                            | otherwise = mynode left ++ [op] ++ mynode right

-- дополнительная функция для подсчета result - по строке "1+2+3-45.." высчитать результат
result2 :: [String] -> Integer
result2 [] = 0
result2 (x:ls) | length (splitOn "-" x) /= 1 = -minus (splitOn "-" x) + result2 ls
              | otherwise = (read x :: Integer) + result2 ls

-- дополнительная функция для result - для минуса
minus :: [String] -> Integer
minus [] = 0
minus (x:ls) = - (read x) - (minus ls)

-- дополнительная функция для result - для плюса
plus :: [String] -> Integer
plus = foldr (\ x -> (+) (read x :: Integer)) 0

-- по строке "1+2+3-45.." высчитать результат
result :: String -> Integer
result x | x == "123456789" = 123456789
         | length (splitOn "-" x) == 1 = plus (splitOn "+" x)
         | length (splitOn "+" x) == 1 = (read (head (splitOn "-" x)) :: Integer) - plus (tail (splitOn "-" x))
         | otherwise = result2 (splitOn "+" x)

-- ищем по индексу ответ в сгененированном списке gen
findIndex p xs = case [ i | (x, i) <- zip (map result xs) [0..], p x ] of
    [] -> Nothing
    e:_ -> Just (xs !! e)

-- результат
operations :: Integer -> Maybe String
operations p = findIndex (== p) (map mynode (gen [1..9]))

-- ПРОВЕРКА

-- operations 123: Just "1+2+34+5-6+78+9"
-- operations 30: Just "12+3+4-5+6-7+8+9"
-- operations 100: Just "1+23-4+56+7+8+9"
-- operations 2013: Nothing