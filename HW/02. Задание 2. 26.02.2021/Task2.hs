data Tree key value = Empty | 
                      Node key (Tree key value) (Tree key value) value


height  ::  Tree key value -> Int
height Empty           =  0 -- если конструктором Empty, то 0
height (Node _ t1 t2 _)  =  1 + max (height t1) (height t2) -- если другим, то это


get :: (Ord k) => k -> Tree k v -> v
get x Empty = error "No key in Binary Tree"
get x (Node key l r value)
    | x < key     = get x l
    | x > key     = get x r
    | otherwise = value
 

put :: Ord k => (k, v) -> Tree k v -> Tree k v
put (k, v) Empty = Node k Empty Empty v
put (k, v) (Node key left right value)
    | k < key     = Node key (put (k, v) left) right value
    | k > key     = Node key left (put (k, v) right) value
    | otherwise = Node key left right v


remove :: Ord k => k -> Tree k v -> Tree k v
remove k Empty = error "Can't delete: No key in Binary Tree"
remove k (Node key left right value)
    | k < key     = Node key (remove k left) right value
    | k > key     = Node key left (remove k right) value
    | otherwise = Node k Empty Empty value
    

keys :: Tree k v -> [k]
keys Empty = []
keys (Node key t1 t2 _)  =  key:keys t1++keys t2


values :: Tree k v -> [v]
values Empty = []
values (Node _ t1 t2 value)  =  value:values t1++values t2


pairsBy :: (k -> Bool) -> Tree k v -> [(k, v)] 
pairsBy _ Empty = []
pairsBy f (Node key left right value) | f key = (key, value):pairsBy f left++pairsBy f right
                                      | otherwise = pairsBy f left++pairsBy f right