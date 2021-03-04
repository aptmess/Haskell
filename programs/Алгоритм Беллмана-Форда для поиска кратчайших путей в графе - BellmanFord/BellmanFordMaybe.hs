module BellmanFordMaybe where

{-
 Реализация алгоритма Беллмана - Форда поиска всех кратчайших путей
 из заданной вершины до всех прочих вершин в графе (для числовых нагрузок на дуги)
 -}

import Graph
import Data.Maybe

type ArcList w = [Arc w]         -- список дуг
type Weight w = (Int -> Maybe w) -- весовая функция на вершинах

-- Изменение весовой нагрузки для одной вершины.
--  - u        - номер вершины
--  - new      - новое значение весовой функции для этой вершины
--  - weights  - весовая функция
replace :: Int -> w -> Weight w -> Weight w
replace u new weights = \v -> if u == v then Just new else weights v

-- Создание начальной весовой функции для заданной вершины
initDistance :: Num w => Int -> Graph w -> Weight w
initDistance u gr = replace u 0 (\v -> Nothing)

-- Расстояния до всех вершин графа согласно заданной вестовой функции
distances :: Graph w -> Weight w -> [Maybe w]
distances g weights = map weights [0..verts g - 1]

-- Алгоритм релаксации дуги
-- Результат работы функции - пара (b, w),
-- где b - признак изменения веса; w - новая весовая функция
relax :: Real w => Weight w -> Arc w -> (Bool, Weight w)
relax weights arc =
       if isJust wFrom &&
          (isNothing wTo || fromJust wFrom + w < fromJust wTo) then
            (True, replace t (fromJust wFrom + w) weights) else
            (False, weights)
    where f = from arc
          t = to arc
          w = weight arc
          wFrom = weights f
          wTo = weights t

-- Реализация одного цикла алгоритма Беллмана - Форда: релаксация всех дуг.
-- В результате - новая весовая функци и признак того, что хотя бы один вес изменился.
bfCycle :: Real w => Weight w -> Graph w -> (Bool, Weight w)
bfCycle weights g = gfold func (False, weights) g
    where func (ch, weight) arc = (ch || newCh, newWeight)
              where (newCh, newWeight) = relax weight arc

-- Алгоритм Беллмана - Форда. Остановка после n шагов или если ничего не меняется.
-- Результат - 
--    пустой список, если кратчайших путей не существует (цикл с отрицательном весом есть в графе)
--    список кратчайших расстояний, если циклов с отрицательным весом нет.
bellmanFord :: (Real w, Show w) => Int -> Graph w -> [Maybe w]
bellmanFord start gr = if changed then [] else distances gr final
    where (changed, _, final) = steps (0, initDistance start gr)
          steps (k, weights) | k > n = (True, n, weights)
                             | ch = steps (k+1, newWeights)
                             | otherwise = (False, k, weights)
              where (ch, newWeights) = bfCycle weights gr
          n = verts gr

--------------------------------------
-- Несколько вариантов тестовых графов
--------------------------------------
testGraph :: Graph Int
testGraph =
    addArc (0,2,1)  $ addArc (0,2,2) $
    addArc (1,-1,2) $ addArc (1,2,3) $
    addArc (2,2,3)  $ addArc (2,2,4) $
    addArc (3,-1,4) $ addArc (3,2,5) $
    addArc (4,2,5)  $ addArc (4,2,0) $
    addArc (5,-1,0) $ addArc (5,2,1) $
    createGraph 6

testGraph1 :: Int -> Graph Integer
testGraph1 n = foldr addArcs3
                  (foldr addArcs2
                      (foldr addArcs1 initGraph [0..n-1])
                      [0,2..n-2])
                  [1,3..n-1]
    where initGraph = createGraph n
          addArcs1 k gr = addArc (k, 2, (k+2) `mod` n) gr
          addArcs2 k gr = addArc (k, 2, (k+1) `mod` n) gr
          addArcs3 k gr = addArc (k, -1, (k+1) `mod` n) gr
