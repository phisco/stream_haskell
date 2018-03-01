import Pipes

pair :: ListT IO (Int, Int)
pair = do
    x <- Select $ each [1, 2]
    lift $ putStrLn $ "x = " ++ show x
    y <- Select $ each [3, 4]
    lift $ putStrLn $ "y = " ++ show y
    return (x, y)
