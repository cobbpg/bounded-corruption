{-# LANGUAGE PackageImports, OverloadedStrings, DataKinds #-}

import Control.Monad
import Control.Monad.Fix
import Data.Bitmap.IO
import Data.Time.Clock
import qualified Data.Trie as T
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MV
import Graphics.Text.TrueType
import "GLFW-b" Graphics.UI.GLFW as GLFW
import LambdaCube.Font.Atlas
import LambdaCube.Font.Common
import LambdaCube.Font.CompositeDistanceField
import LambdaCube.GL
import LambdaCube.GL.Mesh
import System.Environment

data SpreadState
    = I
      { currentIntensity :: MV.IOVector Float
      , previousIntensity :: MV.IOVector Float
      , spreadFactor :: MV.IOVector Float
      }

windowWidth, windowHeight :: Num n => n
windowWidth = 1024
windowHeight = 768

size :: Int
size = 64

sendFactor :: Float
sendFactor = 0.125

receiveFactor :: Float
receiveFactor = 0.25

stepLength :: Float
stepLength = 1 / 30

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

isComplete :: SpreadState -> IO Bool
isComplete (I ci _ sf) = do
    let accum sum ix = do
            cur <- MV.unsafeRead ci ix
            fac <- MV.unsafeRead sf ix
            return $! sum + if fac < 0 || cur > 0.08 then 1 else 0
    sum <- foldM accum 0 [0..size * size - 1]
    return $ sum >= fromIntegral (size * size) * 0.999

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

textStyle = defaultTextStyle { textLetterSpacing = 0.0, textLineHeight = 1.25 }
fontOptions = defaultOptions { atlasSize = 1024, atlasLetterPadding = 2 }

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

    Right font <- loadFontFile "ArkitechLight.ttf"
    let letterScale = 64
    atlas <- createFontAtlas font fontRenderer fontOptions { atlasLetterScale = letterScale }

    renderer <- compileRenderer (ScreenOut (PrjFrameBuffer "" tix0 renderInfection))
    setScreenSize renderer windowWidth windowHeight
    let uniforms = uniformSetter renderer
        aspect = fromIntegral windowHeight / fromIntegral windowWidth
        letterScale = atlasLetterScale (atlasOptions atlas)
        letterPadding = atlasLetterPadding (atlasOptions atlas)

    uniformFTexture2D "fontAtlas" uniforms (getTextureData atlas)

    textMesh <- buildTextMesh atlas textStyle ""
    textBuffer <- compileMesh textMesh
    textObject <- addMesh renderer "textMesh" textBuffer []
    let textScale = 0.15
    uniformM33F "textTransform" uniforms (V3 (V3 (textScale * aspect) 0 0) (V3 0 textScale 0) (V3 (-aspect) (-1 + textScale * 0.1) 1))
    uniformFloat "outlineWidth" uniforms (min 0.5 (fromIntegral letterScale / (windowHeight * fromIntegral letterPadding * textScale * sqrt 2 * 0.75)))

    quadBuffer <- compileMesh quadMesh
    quadObject <- addMesh renderer "infectionMesh" quadBuffer []

    infectionMap <- createInfectionTexture
    uniformFTexture2D "infectionMap" uniforms infectionMap
    uniformM33F "infectionTransform" uniforms (V3 (V3 (2 * aspect) 0 0) (V3 0 2 0) (V3 (-aspect) (-1) 1))

    let playLevel path = do
            gameState <- loadLevel path
            startTime <- getCurrentTime
            flip fix (startTime, 0, 0, False, textObject) $ \loop (prevTime, stepTime, powerUsed, mouseWasPressed, oldTextObject) -> do
                curTime <- getCurrentTime
                complete <- isComplete gameState
                let stepTime' = stepTime + realToFrac (diffUTCTime curTime prevTime)
                    stepping = not complete && stepTime' > stepLength
                    nextStepTime = if stepping then stepTime' - stepLength else stepTime'
                when stepping $ do
                    forM_ [0, 0.2, 0.4] (spreadInfection gameState)
                    updateInfectionTexture infectionMap gameState
                textMesh <- buildTextMesh atlas textStyle ("POWER: " ++ show powerUsed)
                textBuffer <- compileMesh textMesh
                removeObject renderer oldTextObject
                newTextObject <- addMesh renderer "textMesh" textBuffer []
                render renderer
                swapBuffers mainWindow
                pollEvents
                escPressed <- keyIsPressed Key'Escape
                mouseState <- getMouseButton mainWindow MouseButton'1
                let mousePressed = mouseState == MouseButtonState'Pressed
                    mouseClicked = mousePressed && not mouseWasPressed
                when mouseClicked $ do
                    (mx, my) <- getCursorPos mainWindow
                    let x = round ((mx - 128) / 12)
                        y = 63 - round (my / 12)
                    infectSpot gameState x y 10
                let powerUsed' = powerUsed + (if stepping then 1 else 0) + (if mouseClicked then 100 else 0)
                if escPressed || complete
                    then removeObject renderer newTextObject >> return powerUsed
                    else loop (curTime, nextStepTime, powerUsed', mousePressed, newTextObject)

    let showMenu = do
            textObjects <- forM [1..3] $ \n -> do
                textMesh <- buildTextMesh atlas textStyle ("Level " ++ show n)
                textBuffer <- compileMesh textMesh
                addMesh renderer "textMesh" textBuffer ["textTransform", "outlineWidth"]
            let textScale = 0.25
                itemCount = length textObjects
            startTime <- getCurrentTime
            chosenItem <- flip fix (startTime, 0, replicate itemCount 0, False, False) $ \loop (prevTime, n, xs, upWasPressed, downWasPressed) -> do
                curTime <- getCurrentTime
                forM_ (zip3 textObjects [0..] xs) $ \(textObject, index, x) -> do
                    let textUniforms = objectUniformSetter textObject
                        textScale' = textScale + x * 0.03
                    uniformM33F "textTransform" textUniforms (V3 (V3 (textScale' * aspect) 0 0) (V3 0 textScale' 0) (V3 (-aspect + x * 0.1) (-1 + textScale * (0.1 + 5 - index)) 1))
                    uniformFloat "outlineWidth" textUniforms (min 0.5 (fromIntegral letterScale / (windowHeight * fromIntegral letterPadding * textScale' * sqrt 2 * 0.75)))
                render renderer
                swapBuffers mainWindow
                pollEvents
                [escPressed, enterPressed, upPressed, downPressed] <- mapM keyIsPressed [Key'Escape, Key'Enter, Key'Up, Key'Down]
                let n' = (n - (if not upWasPressed && upPressed then 1 else 0) + (if not downWasPressed && downPressed then 1 else 0)) `mod` itemCount
                    xs' = zipWith moveItem xs [0..]
                    moveItem x index
                        | index == n = min 1 (x + dt * 5)
                        | otherwise  = max 0 (x - dt * 5)
                    dt = realToFrac (diffUTCTime curTime prevTime)
                case () of
                    _ | enterPressed -> return n
                      | escPressed -> return (-1)
                      | otherwise -> loop (curTime, n', xs', upPressed, downPressed)
            mapM_ (removeObject renderer) textObjects
            return chosenItem

    let levels = ["level1.txt", "level2.txt", "level3.txt"]
    fix $ \loop -> do
        chosenItem <- showMenu
        if chosenItem < 0
            then return ()
            else playLevel (levels !! chosenItem) >> loop

    destroyWindow mainWindow
    terminate

renderInfection :: Exp Obj (FrameBuffer 1 V4F)
renderInfection = renderText (renderQuad emptyBuffer)
  where
    renderText = Accumulate textFragmentCtx PassAll textFragmentShader textFragmentStream
    renderQuad = Accumulate quadFragmentCtx PassAll quadFragmentShader quadFragmentStream
    emptyBuffer = FrameBuffer (ColorImage n1 (V4 0 0 0 1) :. ZT)
    rasterCtx = TriangleCtx CullNone PolygonFill NoOffset LastVertex

    textFragmentCtx = AccumulationContext Nothing (ColorOp textBlending (V4 True True True True) :. ZT)
    textBlending = Blend (FuncAdd, FuncAdd) ((One, One), (OneMinusSrcAlpha, One)) zero'
    textFragmentStream = Rasterize rasterCtx textStream
    textStream = Transform (vertexShader "textTransform") (Fetch "textMesh" Triangles (IV2F "position", IV2F "uv"))

    quadFragmentCtx = AccumulationContext Nothing (ColorOp NoBlending (V4 True True True True) :. ZT)
    quadFragmentStream = Rasterize rasterCtx quadStream
    quadStream = Transform (vertexShader "infectionTransform") (Fetch "infectionMesh" Triangles (IV2F "position", IV2F "uv"))

    vertexShader transName attr = VertexOut point (floatV 1) ZT (Smooth uv :. ZT)
      where
        point = v3v4 (transform @*. v2v3 pos)
        transform = Uni (IM33F transName) :: Exp V M33F
        (pos, uv) = untup2 attr

    textFragmentShader uv = FragmentOut (pack' (V4 result result result result) :. ZT)
      where
        result = step distance
        distance = sampleDistance "fontAtlas" uv
        step = smoothstep' (floatF 0.5 @- outlineWidth) (floatF 0.5 @+ outlineWidth)
        outlineWidth = Uni (IFloat "outlineWidth") :: Exp F Float

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
