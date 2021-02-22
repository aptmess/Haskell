newtype Node a = Node (Maybe a, [Arc a])
newtype Arc a = Arc (Char, Node a)

flatten :: [[a]] -> [a]
flatten lst = foldl (++) [] lst

getEntries'' :: String -> Arc a -> [(String, a)]
getEntries'' key (Arc (k, node)) = getEntries' (key ++ [k]) node

getEntries' :: String -> Node a -> [(String, a)]
getEntries' key (Node (Just value, arcs)) = (key, value) : (flatten  (map (getEntries'' key) arcs))
getEntries' key (Node (Nothing, arcs)) = flatten (map (getEntries'' key) arcs)

getEntries :: Node a -> [(String, a)]
getEntries node = getEntries' "" node

occurArcs :: Char -> [Arc a] -> Bool
occurArcs t arcs = foldl (||) False (map (occurArcs' t) arcs)

occurArcs' :: Char -> Arc a -> Bool
occurArcs' t (Arc (key, value)) = t == key

add' :: (String, Maybe a) -> Arc a -> Arc a
add' ((x:ls), value) arc@(Arc (k, node)) | x == k    = Arc (k, add (ls, value) node)
                                         | otherwise = arc

add :: (String, Maybe a) -> Node a -> Node a
add ([], value) (Node (_, arcs)) = Node (value, arcs)
add pair@((x:ls), value) (Node (paper, arcs)) | occurArcs x arcs = Node (paper, map (add' pair) arcs)
                                              | otherwise        = Node (paper, (Arc (x, add (ls, value) (Node (Nothing, [])) )):arcs)