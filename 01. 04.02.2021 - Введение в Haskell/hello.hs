main = do
    print "My first Haskell programm"
    name <- getLine
    print ("Hello, " ++name)

f x y = x + y