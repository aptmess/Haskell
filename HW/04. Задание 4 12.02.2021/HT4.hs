-- TASK1

-- all_pairs - генератор взаимно простых чисал
-- sort_pairs -- по сумме сортируем
pairs :: [(Integer,Integer)]
pairs = sort_pairs 0 where 
        sort_pairs n = all_pairs n ++ sort_pairs (n + 1)
        all_pairs n = [ (x, y) | x <- [2..n], let y = n - x, gcd x y  == 1, x < y]
        
-- проверка:
    -- take 12 pairs

-- TASK2

buildDiag :: [a] -> [[a]]
buildDiag list = map (map (list!!)) (matrix 0 1) where
            matrix value diff = row value diff : matrix (value + diff + 1) (diff + 1) -- создаём матрицу индексов
            row value diff = value : row (value + diff) (diff+1) -- каждый раз разницу увеличиваем на 1
            
-- проверка:
    -- let result = buildDiag (map (^2) [1..])
    -- mapM_ print $ (map (take 3) (take 3 result))


-- TASK3

pascal :: [[Integer]]
pascal = [1]: map (\l -> zipWith (+) (0:l) (l ++ [0])) pascal

-- проверка: 
    -- mapM_ print $ take 11 pascal
    

-- TASK4

-- Я бы предложил следующий алгоритм:

-- - выбираю максимальный элемент
-- - кладу в вершину Tree a
-- - далее разделяю оставшийся список на две части (без максимального элемента) - одну часть кладу в левого потомка кучи, вторую часть - в -- правого потомка кучи

-- Есть и другой алгоритм: перестройка двоичного дерева в кучу по тройкам элементов. Но!

-- *В двоичной куче* в вершине должен стоять максимальный элемент, как в бесконечном списке можно найти максимум? Мне это кажется очень странным. Поэтому предположим, что у нас список упорядочен по убыванию, хотя это и странно. Иначе - решения нет.

data Tree a = Tree a (Tree a) (Tree a)
    deriving Show

-- построение по спику упорядоченному по убыванию!!
buildHeap :: [a] -> Tree a
buildHeap (h:ls) = Tree h (buildHeap (map (ls!!) [1, 3..])) (buildHeap (map (ls!!) [0, 2..]))

-- бексонечное двоичное дерево, возможно, поможет для перестройки в двоичную кучу..
heap :: Integer -> Tree Integer
heap n = Tree n (heap (2*n)) (heap (2*n + 1)) 