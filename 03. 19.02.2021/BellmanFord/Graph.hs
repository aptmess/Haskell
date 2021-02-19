module Graph(Graph,Arc,verts,arcs,from,to,weight,
    createGraph,addArc,gmap,gfold) where

{-
 Представление нагруженного графа как абстрактного типа данных.
 -}

-- Граф представлен списками смежности с нагрузками на дугах
newtype Graph w = Graph [[(w, Int)]] deriving Show
newtype Arc w = Arc (Int, w, Int)    -- w - нагрузка на дугу

-- Количество вершин графа
verts :: Graph w -> Int
verts (Graph list) = length list

-- Все дуги графа
arcs :: Graph w -> [Arc w]
arcs g@(Graph list) = concat $ zipWith addStart [0..n-1] list
    where n = verts g
          addStart u list = map (\(w, v) -> Arc (u, w, v)) list

-- Исходящая вершина дуги
from :: Arc w -> Int
from (Arc (u, _, _)) = u

-- Входящая вершина дуги
to :: Arc w -> Int
to (Arc (_, _, v)) = v

-- Нагрузка на дуге
weight :: Arc w -> w
weight (Arc (_, w, _)) = w

-- Создание пустого графа с n вершинами
createGraph :: Int -> Graph w
createGraph n = Graph $ replicate n []

-- Добавление дуги к графу
addArc :: (Int, w, Int) -> Graph w -> Graph w
addArc (from, w, to) (Graph list)
        | from < 0 || to < 0 || from >= n || to >= n =
             error "Vertex does not exist"
        | otherwise = Graph $ prev ++ (((w, to) : arcs) : next)
    where n = length list
          (prev, arcs:next) = splitAt from list

-- Функция map для графа: структура графа не меняется, меняются нагрузки на дугах
gmap :: (v -> w) -> Graph v -> Graph w
gmap f (Graph list) = Graph $ map mapFunc list
    where mapFunc arcs = map (\(w, v) -> (f w, v)) arcs

-- Свертка (обход) дуг графа. Порядок обхода не определен
gfold :: (b -> Arc w -> b) -> b -> Graph w -> b
gfold f seed g = foldl f seed $ arcs g
