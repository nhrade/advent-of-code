import           System.IO

-- Count increasing number of elements in a list
countIncreasing :: [Int] -> Int
countIncreasing []  = 0
countIncreasing [x] = 0
countIncreasing (x : y : xs) =
    if x < y then 1 + countIncreasing (y : xs) else countIncreasing (y : xs)


-- Calculate sliding sum of 3 elements
sumSlidingWindow :: [Int] -> [Int]
sumSlidingWindow [] = []
sumSlidingWindow ls = [sum $ take 3 ls] ++ sumSlidingWindow (drop 1 ls)


main = do
    withFile
        "input.txt"
        ReadMode
        (\handle -> do
            contents <- hGetContents handle

            -- turn contents into a list of numbers
            let numbers           = map read $ lines contents :: [Int]
            let increasing        = countIncreasing numbers

            let increasingSliding = countIncreasing $ sumSlidingWindow numbers
            -- putStr $ show increasing ++ "\n"
            putStr $ show increasingSliding
        )
