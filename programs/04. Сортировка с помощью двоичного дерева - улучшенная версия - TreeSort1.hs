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
-- Правая свертка дерева
foldt   :: (a -> b -> b) -> b -> Tree a -> b

-- Реализация функций
insert e Empty                    =  Node Empty e Empty
insert e (Node t1 n t2) | e < n   =  Node (insert e t1) n t2
                        | e >= n  =  Node t1 n (insert e t2)

foldt _ seed Empty = seed
foldt f seed (Node t1 root t2) = foldt f (f root (foldt f seed t2)) t1

sort       =  flatten . build
build      =  foldr insert Empty
flatten    =  foldt (:) []
