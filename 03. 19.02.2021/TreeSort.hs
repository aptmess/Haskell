module TreeSort where

-- Определение двоичного дерева
data Tree a = Empty |
              Node (Tree a) a (Tree a)
              
-- Сортировка массива путё построения двоичного дерева поиска.              
sort    :: (Ord a) => [a] -> [a]
-- Построение дерева
build   :: (Ord a) => [a] -> Tree a
-- Вставка элемента в дерево поиска
insert  :: (Ord a) => a -> Tree a -> Tree a
-- Обход дерева с заполнением списка его элементами
flatten :: Tree a -> [a]

-- Реализация функций
sort ls       =  flatten (build ls)

build []      =  Empty
build (e:ls)  =  insert e (build ls)

insert e Empty                    =  Node Empty e Empty
insert e (Node t1 n t2) | e < n   =  Node (insert e t1) n t2
                        | e >= n  =  Node t1 n (insert e t2)

flatten Empty = []
flatten (Node t1 n t2) = (flatten t1) ++ (n : (flatten t2))
