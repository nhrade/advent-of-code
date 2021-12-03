import           System.IO


move :: (Int, Int) -> (String, Int) -> (Int, Int)
move (x, y) pair = case pair of
    ("forward", n) -> (x + n, y)
    ("up"     , n) -> (x, y - n)
    ("down"   , n) -> (x, y + n)
    (_        , n) -> (x, y)

driveSubmarine :: [(String, Int)] -> (Int, Int) -> (Int, Int)
driveSubmarine commands initPos = foldl move initPos commands

main :: IO ()
main = do
    withFile "input.txt" ReadMode $ \handle -> do
        contents <- hGetContents handle
        let input    = map words $ lines contents
        -- convert list to tuple of string and integer
        let commands = [ (x !! 0, read (x !! 1) :: Int) | x <- input ]
        let finalPos = driveSubmarine commands (0, 0)
        print finalPos
        -- get second item of finalPOs
        let prod = (fst finalPos) * (snd finalPos) in print prod
