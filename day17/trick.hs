import           System.IO

type Vector = (Int, Int)

data Probe = Probe
    { position :: Vector
    , velocity :: Vector
    }
    deriving (Show, Eq)

targetX :: Vector
targetX = (29, 73)
targetY :: Vector
targetY = (-248, -194)

step :: Probe -> Probe
step Probe { position = (x, y), velocity = (vx, vy) } = Probe
    { position = (x + vx, y + vy)
    , velocity = (newVx vx, newVy vy)
    }
  where
    newVx 0  = 0
    newVx vx = if vx < 0 then vx + 1 else vx - 1
    newVy vy = vy - 1

iterationLimit :: Int
iterationLimit = 500

checkInRange :: Probe -> Bool
checkInRange Probe { position = (x, y) } =
    x >= fst targetX && x <= snd targetX && y >= fst targetY && y <= snd targetY

checkGTRange :: Probe -> Bool
checkGTRange Probe { position = (x, y) } = x > snd targetX && y > snd targetY

doesLandInRange :: Vector -> Bool
doesLandInRange initVel = not (null rest)
  where
    (fw, rest) =
        break checkInRange $ take iterationLimit $ iterate step initState
    initState = Probe { position = (0, 0), velocity = initVel }

countAllValidSolutions :: Int
countAllValidSolutions =
    length
        $ filter (== True)
        $ [ doesLandInRange (vx, vy)
          | vx <- [1 .. 500]
          , vy <- [-(abs (fst targetY)) .. abs (fst targetY)]
          ]

findHighestY :: Int
findHighestY = n * (n + 1) `div` 2 where n = -(fst targetY) - 1

main :: IO ()
main = do
    putStrLn $ "Highest Y: " ++ show findHighestY
    print $ countAllValidSolutions