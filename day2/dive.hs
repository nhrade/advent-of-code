import           System.IO


move :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
move (x, y, aim) pair = case pair of
    ("forward", n) -> (x + n, y + aim * n, aim)
    ("up"     , n) -> (x, y, aim - n)
    ("down"   , n) -> (x, y, aim + n)
    (_        , n) -> (x, y, aim)

driveSubmarine :: [(String, Int)] -> (Int, Int, Int) -> (Int, Int, Int)
driveSubmarine commands initPos = foldl move initPos commands

main :: IO ()
main = do
    withFile "input.txt" ReadMode $ \handle -> do
        contents <- hGetContents handle
        let input    = map words $ lines contents
        -- convert list to tuple of string and integer
        let commands = [ (x !! 0, read (x !! 1) :: Int) | x <- input ]
        let finalPos = driveSubmarine commands (0, 0, 0)
        print finalPos
        -- get second item of finalPOs
        -- get first and second item of finalPos

        let 
            (x, y, _) = finalPos
            prod = x * y in print prod
