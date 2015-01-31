import Control.Monad
import qualified Data.Vector.Storable.Mutable as MV
import System.Environment

data SpreadState
    = I
      { currentIntensity :: MV.IOVector Float
      , previousIntensity :: MV.IOVector Float
      , spreadFactor :: MV.IOVector Float
      }

size :: Int
size = 64

sendFactor :: Float
sendFactor = 0.125

receiveFactor :: Float
receiveFactor = 0.25

(@^) :: Int -> Int -> Int
x @^ y = x + y * size

createEmptyState :: IO SpreadState
createEmptyState = do
    ci <- MV.replicate (size * size) 0
    pi <- MV.replicate (size * size) 0
    sf <- MV.replicate (size * size) 0
    return (I ci pi sf)

loadLevel :: FilePath -> IO SpreadState
loadLevel path = do
    levelData <- readFile path
    s <- createEmptyState
    forM_ (zip (lines levelData) [0..]) $ \(line, y) -> do
        forM_ (zip line [0..]) $ \(char, x) -> do
            case char of
                '#' -> conditionSpot s x y (-1)
                _ -> return ()
    return s

infectSpot :: SpreadState -> Int -> Int -> Float -> IO ()
infectSpot (I ci _ _) x y s = do
    MV.unsafeWrite ci (x @^ y) s

conditionSpot :: SpreadState -> Int -> Int -> Float -> IO ()
conditionSpot (I _ _ sf) x y s = do
    MV.unsafeWrite sf (x @^ y) s

spreadInfection :: SpreadState -> Float -> IO ()
spreadInfection (I ci pi sf) threshold = do
    MV.unsafeCopy pi ci
    forM_ [0..size - 1] $ \y -> forM_ [0..size - 1] $ \x -> do
        cur <- MV.unsafeRead pi (x @^ y)
        fac <- MV.unsafeRead sf (x @^ y)
        let neighs = filter freeSpot [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
            freeSpot (x, y) = x >= 0 && x < size && y >= 0 && y < size
        ns <- mapM (\(x, y) -> MV.unsafeRead pi (x @^ y)) neighs
        let difs = map dif ns
            dif n = if abs d < threshold then 0 else if d > 0 then d * receiveFactor else d * sendFactor
              where
                d = n - cur
        MV.unsafeWrite ci (x @^ y) (max 0 (cur + sum difs * (1 + fac)))

printState :: SpreadState -> IO ()
printState (I ci _ sf) = do
    putStr "\ESC[0;0H"
    forM_ [0..size - 1] $ \y -> do
        forM_ [0..size - 1] $ \x -> do
            cur <- MV.unsafeRead ci (x @^ y)
            fac <- MV.unsafeRead sf (x @^ y)
            putStr ("\ESC[48;5;" ++ show (ansiColor cur (fac + 0.5)) ++ "m \ESC[0m")
        putStrLn ""
  where
    ansiColor c1 c2 = 16 + component c1 * 36 + component c2
      where
        component c = min 5 (max 0 (round (c * 5)))

main :: IO ()
main = do
    args <- getArgs
    s <- case args of
        [] -> do
            s <- createEmptyState
            forM_ [4..27] $ \x -> conditionSpot s x 15 (1)
            forM_ [4..27] $ \x -> conditionSpot s x 14 (0.5)
            forM_ [16..30] $ \y -> conditionSpot s 15 y (-1)
            return s
        (path:_) -> do
            loadLevel path
    infectSpot s 4 7 3
    infectSpot s 36 21 10
    putStr "\ESC[2J"
    forever $ do
        printState s
        forM_ [0, 0.2, 0.4, 0.8] (spreadInfection s)
        putStrLn ""
