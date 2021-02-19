import Data.Maybe(isNothing, fromJust)
import Data.List(union, (\\))

newtype Graph = Graph [[Int]]

-- Дерево путей в процессе обхода графа в ширину.
type Tree = Int -> Maybe Int

-- Дуга в графе.
type Arc = (Int, Int)

-- Поиск пути в дереве обхода до заданной вершины в предположении,
-- что заданная вершина достигнута в процессе обхода.
getPath :: Int -> Tree -> [Int]
getPath v tree | isNothing (tree v) = [v]
               | otherwise = v : getPath (fromJust (tree v)) tree

-- Добавление дуги в дерево обхода
add :: Arc -> Tree -> Tree
add (u, v) tree arg = if arg == v then Just u else tree arg

-- Обход графа в ширину.
-- Аргументы:
--    finish - вершина, достижение которой является целью;
--    passed - множество пройденных вершин;
--    front  - фронт волны;
--    tree   - текущее дерево обхода;
--    gr     - граф.
-- Результат:
--    дерево обхода до целевой вершины, если эта вершина достижима.
bfs :: Int -> [Int] -> [Int] -> Tree -> Graph -> Maybe Tree
bfs finish passed front tree gr@(Graph list)
    | null front = Nothing
    | finish `elem` front = Just tree
    | otherwise = bfs finish newPassed newFront newPath gr
  where newPassed = passed `union` front
        (newFront, newPath) =
            foldr insertArcs ([], tree) $ map getArcs front
        getArcs :: Int -> [Arc]
        getArcs u = map ((,) u) $ list!!u \\ newPassed
        insertArcs :: [Arc] -> ([Int], Tree) -> ([Int], Tree)
        insertArcs s (u, p) = (u `union` (map snd s), foldr add p s)

-- Поиск пути в графе от начальной до целевой вершины.
-- Аргументы:
--    start  - начальная вершина;
--    finish - целевая вершина;
--    gr     - исходный граф.
findPath :: Int -> Int -> Graph -> Maybe [Int]
findPath start finish gr = case bfs finish [] [start] initTree gr of
    Nothing -> Nothing
    Just tree -> Just $ reverse $ getPath finish tree

-- Начальное пустое дерево обхода.
initTree :: Tree
initTree u = Nothing

-----------------------------------------------
-- Тестовые графы
-----------------------------------------------

testGraph :: Graph
testGraph = Graph [[1,2],[],[1,5,6],[0],[1],[1,4,6,7],[3],[6]]

testGraph1 :: Int -> Graph
testGraph1 n = Graph $ map (\u -> [u+1,u+2]) [0..n-3] ++ [[n-1],[]]

testGraph2 :: Int -> Graph
testGraph2 n = Graph $ map (\u -> [u+1]) [0..n-2] ++ [[]]

main = do
    print $ map (\(from, to) -> findPath from to $ testGraph1 12) [(0, 10), (1, 10), (10, 10), (10, 0)]
    print $ map (\(from, to) -> findPath from to $ testGraph2 12) [(0, 10), (1, 10), (10, 10), (10, 0)]
    print $ map (\from -> findPath from 7 testGraph) [0..7]
    