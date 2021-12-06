import           Data.List
import           System.IO

commonBit :: [Char] -> Bool -> Char
commonBit bits mostCommon =
    let count (count0, count1) x =
            if x == '1' then (count0, count1 + 1) else (count0 + 1, count1)
        totalCount = foldl count (0, 0) bits
    in  case mostCommon of
            True  -> if (fst totalCount) > snd totalCount then '0' else '1'
            False -> if (fst totalCount) < snd totalCount then '0' else '1'


commonBitsAllColumns :: [String] -> Bool -> [Char]
commonBitsAllColumns rows mostCommon =
    map (\r -> commonBit r mostCommon) $ transpose rows

binaryToDecimal :: [Char] -> Int
binaryToDecimal bits =
    foldl (\acc x -> acc * 2 + (if x == '1' then 1 else 0)) 0 bits


powerConsumption :: [String] -> Int
powerConsumption rows =
    let gammaRate   = binaryToDecimal $ commonBitsAllColumns rows True
        epsilonRate = binaryToDecimal $ commonBitsAllColumns rows False
    in  gammaRate * epsilonRate

filterBasedOnBit :: [String] -> Char -> [String]
filterBasedOnBit rows bit = filter (\r -> head r == bit) rows

{-
filterOverColumns :: [String] -> Int -> [String]
filterOverColumns rows column =
    | column > length (transpose rows) = []
    | otherwise = 
        let mcb = (commonBitsAllColumns rows True) !! column
        in filterBasedOnBit rows mcb
-}

oxygenRating :: [String] -> [String]
oxygenRating rows =
    let mcb = (head $ commonBitsAllColumns rows True)
    in  filterBasedOnBit rows mcb


main :: IO ()
main = withFile "input.txt" ReadMode $ \h -> do
    contents <- hGetContents h
    -- get lines of contents
    let contentLines = lines contents
    print $ oxygenRating contentLines
