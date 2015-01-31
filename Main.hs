{-# LANGUAGE PackageImports, OverloadedStrings, DataKinds #-}

import Control.Monad
import Control.Monad.Fix
import Data.Bitmap.IO
import Data.Time.Clock
import qualified Data.Trie as T
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MV
import "GLFW-b" Graphics.UI.GLFW as GLFW
import LambdaCube.Font.Common
import LambdaCube.GL
import LambdaCube.GL.Mesh
import System.Environment

data SpreadState
    = I
      { currentIntensity :: MV.IOVector Float
      , previousIntensity :: MV.IOVector Float
      , spreadFactor :: MV.IOVector Float
      }

windowWidth, windowHeight :: Integral n => n
windowWidth = 1024
windowHeight = 768

size :: Int
size = 64

sendFactor :: Float
sendFactor = 0.125

receiveFactor :: Float
receiveFactor = 0.25

(^*) :: Int -> Int -> Int
x ^* y = x + y * size

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
    forM_ (zip (reverse (lines levelData)) [0..]) $ \(line, y) -> do
        forM_ (zip line [0..]) $ \(char, x) -> do
            case char of
                '#' -> conditionSpot s x y (-1)
                '1' -> conditionSpot s x y (0.15)
                '2' -> conditionSpot s x y (0.3)
                '3' -> conditionSpot s x y (0.45)
                '4' -> conditionSpot s x y (0.6)
                '5' -> conditionSpot s x y (0.75)
                _ -> return ()
    return s

infectSpot :: SpreadState -> Int -> Int -> Float -> IO ()
infectSpot (I ci _ _) x y s = do
    MV.unsafeWrite ci (x ^* y) s

conditionSpot :: SpreadState -> Int -> Int -> Float -> IO ()
conditionSpot (I _ _ sf) x y s = do
    MV.unsafeWrite sf (x ^* y) s

spreadInfection :: SpreadState -> Float -> IO ()
spreadInfection (I ci pi sf) threshold = do
    MV.unsafeCopy pi ci
    forM_ [0..size - 1] $ \y -> forM_ [0..size - 1] $ \x -> do
        cur <- MV.unsafeRead pi (x ^* y)
        fac <- MV.unsafeRead sf (x ^* y)
        let neighs = filter freeSpot [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
            freeSpot (x, y) = x >= 0 && x < size && y >= 0 && y < size
        ns <- mapM (\(x, y) -> MV.unsafeRead pi (x ^* y)) neighs
        let difs = map dif ns
            dif n = if abs d < threshold then 0 else if d > 0 then d * receiveFactor else d * sendFactor
              where
                d = n - cur
        MV.unsafeWrite ci (x ^* y) (min 10 (max 0 (cur + sum difs * (1 + fac))))

createInfectionTexture :: IO TextureData
createInfectionTexture = do
    bmp <- emptyBitmap (size, size) 4 Nothing
    compileTexture2DRGBAF False True (unsafeFreezeBitmap bmp)

updateInfectionTexture :: TextureData -> SpreadState -> IO ()
updateInfectionTexture tex (I ci _ sf) = do
    bytes <- MV.replicate (size * size * 4) 0
    forM_ [0..size * size - 1] $ \ix -> do
        red <- MV.unsafeRead ci ix
        blue <- MV.unsafeRead sf ix
        MV.unsafeWrite bytes (ix * 4) (round (max 0 (min 1 red) * 255))
        MV.unsafeWrite bytes (ix * 4 + 2) (round (max 0 (min 1 (blue + 0.5)) * 255))
    bmp <- MV.unsafeWith bytes $ \ptr -> copyBitmapFromPtr (size, size) 4 0 ptr Nothing
    updateTexture2DRGBAF tex False (unsafeFreezeBitmap bmp)

main :: IO ()
main = do
    GLFW.init
    defaultWindowHints
    mapM_ windowHint
      [ WindowHint'ContextVersionMajor 3
      , WindowHint'ContextVersionMinor 2
      , WindowHint'OpenGLProfile OpenGLProfile'Core
      , WindowHint'OpenGLForwardCompat True
      ]
    Just mainWindow <- createWindow windowWidth windowHeight "Bounded Corruption" Nothing Nothing
    makeContextCurrent (Just mainWindow)

    let keyIsPressed key = do
            keyState <- getKey mainWindow key
            return (keyState == KeyState'Pressed)

    renderer <- compileRenderer (ScreenOut (PrjFrameBuffer "" tix0 renderInfection))
    setScreenSize renderer windowWidth windowHeight
    let uniforms = uniformSetter renderer

    quadBuffer <- compileMesh quadMesh
    quadObject <- addMesh renderer "infectionMesh" quadBuffer []

    infectionMap <- createInfectionTexture
    uniformFTexture2D "infectionMap" uniforms infectionMap

    startTime <- getCurrentTime

    args <- getArgs
    s <- case args of
        [] -> createEmptyState
        (path:_) -> loadLevel path
    fix $ \loop -> do
        forM_ [0, 0.2, 0.4] (spreadInfection s)
        let aspect = fromIntegral windowHeight / fromIntegral windowWidth
        uniformM33F "infectionTransform" uniforms (V3 (V3 (2 * aspect) 0 0) (V3 0 2 0) (V3 (-aspect) (-1) 1))
        updateInfectionTexture infectionMap s
        render renderer
        swapBuffers mainWindow
        pollEvents
        escPressed <- keyIsPressed Key'Escape
        mousePressed <- getMouseButton mainWindow MouseButton'1
        when (mousePressed == MouseButtonState'Pressed) $ do
            (mx, my) <- getCursorPos mainWindow
            let x = round ((mx - 128) / 12)
                y = 63 - round (my / 12)
            infectSpot s x y 10
        unless escPressed loop

    destroyWindow mainWindow
    terminate

renderInfection :: Exp Obj (FrameBuffer 1 V4F)
renderInfection = renderQuad emptyBuffer
  where
    renderQuad = Accumulate quadFragmentCtx PassAll quadFragmentShader quadFragmentStream
    emptyBuffer = FrameBuffer (ColorImage n1 (V4 0 0 0 1) :. ZT)
    rasterCtx = TriangleCtx CullNone PolygonFill NoOffset LastVertex

    quadFragmentCtx = AccumulationContext Nothing (ColorOp NoBlending (V4 True True True True) :. ZT)
    quadFragmentStream = Rasterize rasterCtx quadStream
    quadStream = Transform vertexShader (Fetch "infectionMesh" Triangles (IV2F "position", IV2F "uv"))

    vertexShader attr = VertexOut point (floatV 1) ZT (Smooth uv :. ZT)
      where
        point = v3v4 (transform @*. v2v3 pos)
        transform = Uni (IM33F "infectionTransform") :: Exp V M33F
        (pos, uv) = untup2 attr

    quadFragmentShader uv = FragmentOut (smp :. ZT)
      where
        smp = pack' (V4 inf' z sf' z)
        inf' = Cond (inf @< floatF 0.1) (floatF 0) (floatF 1)
        sf' = Cond (sf @< floatF 0.25) (floatF 0) (round' (sf @* floatF 10) @/ floatF 10)
        V4 inf z sf _ = unpack' (texture' (Sampler LinearFilter Repeat tex) uv)
        tex = TextureSlot "infectionMap" (Texture2D (Float RGBA) n1)

quadMesh :: Mesh
quadMesh = Mesh
    { mAttributes   = T.fromList
        [ ("position", A_V2F (SV.fromList [V2 0 0, V2 1 0, V2 1 1, V2 1 1, V2 0 1, V2 0 0]))
        , ("uv", A_V2F (SV.fromList [V2 0 0, V2 1 0, V2 1 1, V2 1 1, V2 0 1, V2 0 0]))
        ]
    , mPrimitive    = P_Triangles
    , mGPUData      = Nothing
    }

printState :: SpreadState -> IO ()
printState (I ci _ sf) = do
    putStr "\ESC[0;0H"
    forM_ [0..size - 1] $ \y -> do
        forM_ [0..size - 1] $ \x -> do
            cur <- MV.unsafeRead ci (x ^* y)
            fac <- MV.unsafeRead sf (x ^* y)
            putStr ("\ESC[48;5;" ++ show (ansiColor cur (fac + 0.5)) ++ "m \ESC[0m")
        putStrLn ""
  where
    ansiColor c1 c2 = 16 + component c1 * 36 + component c2
      where
        component c = min 5 (max 0 (round (c * 5)))

consoleMain :: IO ()
consoleMain = do
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
        forM_ [0, 0.2, 0.4] (spreadInfection s)
        putStrLn ""
