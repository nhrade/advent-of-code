import System.IO
import Sonar

main = do
    withFile "input.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        
        -- turn contents into a list of numbers
        let numbers = map read $ lines contents :: [Int]
        let increasing = Sonar.countIncreasing numbers

        let increasingSliding = Sonar.countIncreasing $ Sonar.sumSlidingWindow numbers 
        -- putStr $ show increasing ++ "\n"
        putStr $ show increasingSliding)