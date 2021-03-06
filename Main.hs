{-# LANGUAGE PackageImports, OverloadedStrings, DataKinds #-}

import Control.Monad
import Control.Monad.Fix
import Data.Bitmap.IO
import qualified Data.ByteString as BS
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
      , textureBytes :: MV.IOVector Word8
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

uvMapSize :: Num n => n
uvMapSize = 256

(^*) :: Int -> Int -> Int
x ^* y = x + y * size

createEmptyState :: IO SpreadState
createEmptyState = do
    ci <- MV.replicate (size * size) 0
    pi <- MV.replicate (size * size) 0
    sf <- MV.replicate (size * size) 0
    tb <- MV.replicate (size * size * 4) 0
    return (I ci pi sf tb)

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
infectSpot (I ci _ _ _) x y s = do
    MV.unsafeWrite ci (x ^* y) s

conditionSpot :: SpreadState -> Int -> Int -> Float -> IO ()
conditionSpot (I _ _ sf _) x y s = do
    MV.unsafeWrite sf (x ^* y) s

spreadInfection :: SpreadState -> Float -> IO ()
spreadInfection (I ci pi sf _) threshold = do
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
isComplete (I ci _ sf _) = do
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
updateInfectionTexture tex (I ci _ sf tb) = do
    forM_ [0..size * size - 1] $ \ix -> do
        inf <- MV.unsafeRead ci ix
        fac <- MV.unsafeRead sf ix
        MV.unsafeWrite tb (ix * 4) (round (max 0 (min 1 inf) * 255))
        MV.unsafeWrite tb (ix * 4 + 1) (round (max 0 (min 1 (fac + 0.5)) * 255))
    bmp <- MV.unsafeWith tb $ \ptr -> copyBitmapFromPtr (size, size) 4 0 ptr Nothing
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

    let makeSamplerOut = SamplerOut "uvMap" . Sampler LinearFilter Repeat . Texture (Texture2D (Float RGBA) n1) (V2 uvMapSize uvMapSize) NoMip
    uvRenderer <- compileRenderer (makeSamplerOut [PrjFrameBuffer "" tix0 renderUV])
    setScreenSize uvRenderer uvMapSize uvMapSize

    let uniforms = uniformSetter renderer
        uvUniforms = uniformSetter uvRenderer
        aspect = fromIntegral windowHeight / fromIntegral windowWidth
        letterScale = atlasLetterScale (atlasOptions atlas)
        letterPadding = atlasLetterPadding (atlasOptions atlas)
        Just uvMap = T.lookup "uvMap" (samplerOutput uvRenderer)

    uniformFTexture2D "uvMapPrev" uvUniforms uvMap
    quadBuffer <- compileMesh quadMesh
    addMesh uvRenderer "quadMesh" quadBuffer []

    uniformFloat "copyFactor" uvUniforms 0
    render uvRenderer
    uniformFloat "copyFactor" uvUniforms 1
    render uvRenderer

    uniformFTexture2D "uvMap" uniforms uvMap
    uniformFTexture2D "fontAtlas" uniforms (getTextureData atlas)

    let addTextObject text hasOwnUniforms = do
            textMesh <- buildTextMesh atlas textStyle text
            textBuffer <- compileMesh textMesh
            addMesh renderer "textMesh" textBuffer (if hasOwnUniforms then ["textTransform", "outlineWidth", "textOpacity"] else [])
        setTextUniforms uniforms scale opacity x y = do
            uniformM33F "textTransform" uniforms (V3 (V3 (scale * aspect) 0 0) (V3 0 scale 0) (V3 (x - aspect) (y - 1) 1))
            uniformFloat "outlineWidth" uniforms (min 0.5 (fromIntegral letterScale / (windowHeight * fromIntegral letterPadding * scale * sqrt 2 * 0.75)))
            uniformFloat "textOpacity" uniforms opacity

    setTextUniforms uniforms 0.15 1 0 0.015

    quadObject <- addMesh renderer "infectionMesh" quadBuffer []

    infectionMap <- createInfectionTexture
    uniformFTexture2D "infectionMap" uniforms infectionMap
    uniformFTexture2D "infectionMap" uvUniforms infectionMap
    uniformM33F "infectionTransform" uniforms (V3 (V3 (2 * aspect) 0 0) (V3 0 2 0) (V3 (-aspect) (-1) 1))

    let playLevel path = do
            gameState <- loadLevel path
            uniformFloat "copyFactor" uvUniforms 0
            render uvRenderer
            uniformFloat "copyFactor" uvUniforms 1
            startTime <- getCurrentTime
            flip fix (startTime, 0, 0, False, Nothing) $ \loop (prevTime, stepTime, powerUsed, mouseWasPressed, oldTextObject) -> do
                curTime <- getCurrentTime
                complete <- isComplete gameState
                let stepTime' = stepTime + realToFrac (diffUTCTime curTime prevTime)
                    stepping = not complete && stepTime' > stepLength
                    nextStepTime = if stepping then stepTime' - stepLength else stepTime'
                when stepping $ do
                    forM_ [0, 0.2, 0.4] (spreadInfection gameState)
                    updateInfectionTexture infectionMap gameState
                case oldTextObject of
                    Just textObject -> removeObject renderer textObject
                    Nothing -> return ()
                newTextObject <- addTextObject ("POWER: " ++ show powerUsed) False
                render uvRenderer
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
                    then removeObject renderer newTextObject >> return (if escPressed then -1 else powerUsed)
                    else loop (curTime, nextStepTime, powerUsed', mousePressed, Just newTextObject)

    let showMenu = do
            textObjects <- forM [1..3] $ \n -> addTextObject ("Level " ++ show n) True
            let textScale = 0.25
                itemCount = length textObjects
                showBest result = do
                    textObject <- addTextObject ("Best: " ++ if result > 0 then show result else "N/A") True
                    setTextUniforms (objectUniformSetter textObject) 0.2 0.8 0 0.3
                    return textObject
            results <- loadResults itemCount
            bestObject <- showBest (head results)
            startTime <- getCurrentTime
            chosenItem <- flip fix (startTime, 0, replicate itemCount 0, bestObject, False, False) $ \loop (prevTime, n, xs, bestObject, upWasPressed, downWasPressed) -> do
                curTime <- getCurrentTime
                forM_ (zip3 textObjects [0..] xs) $ \(textObject, index, x) -> do
                    setTextUniforms (objectUniformSetter textObject) (0.2 + x * 0.06) (0.5 + x * 0.5) (x * 0.05) (0.2 * (7 - index))
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
                bestObject' <-
                    if n /= n'
                    then do
                        removeObject renderer bestObject
                        showBest (results !! n')
                    else return bestObject
                case () of
                    _ | enterPressed -> removeObject renderer bestObject >> return n
                      | escPressed -> removeObject renderer bestObject >> return (-1)
                      | otherwise -> loop (curTime, n', xs', bestObject', upPressed, downPressed)
            mapM_ (removeObject renderer) textObjects
            return chosenItem

    let levels = ["level1.txt", "level2.txt", "level3.txt"]
    fix $ \loop -> do
        chosenItem <- showMenu
        if chosenItem < 0
            then return ()
            else do
            newResult <- playLevel (levels !! chosenItem)
            results <- loadResults (length levels)
            let previousResult = results !! chosenItem
            when (newResult > 0 && (previousResult < 0 || newResult < previousResult)) $ do
                let results' = zipWith updateResult results [0..]
                    updateResult oldResult index
                        | index == chosenItem = newResult
                        | otherwise           = oldResult
                saveResults results'
            loop

    destroyWindow mainWindow
    terminate

loadResults :: Int -> IO [Int]
loadResults count = do
    resultsText <- BS.readFile "results.txt"
    let resultsText' = map (toEnum . fromIntegral) (BS.unpack resultsText)
        results = take count (map read (lines resultsText') ++ repeat (-1))
    return results

saveResults :: [Int] -> IO ()
saveResults results = do
    writeFile "results.txt" (unlines (map show results))

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
        result = step distance @* opacity
        distance = sampleDistance "fontAtlas" uv
        step = smoothstep' (floatF 0.5 @- outlineWidth) (floatF 0.5 @+ outlineWidth)
        outlineWidth = Uni (IFloat "outlineWidth") :: Exp F Float
        opacity = Uni (IFloat "textOpacity") :: Exp F Float

    quadFragmentShader uv = FragmentOut (smp :. ZT)
      where
        bkgDark = pack' (V4 (floatF 0.12) (floatF 0.15) (floatF 0.1) (floatF 0))
        bkgLight = pack' (V4 (floatF 0.33) (floatF 0.33) (floatF 0.55) (floatF 0))
        bkg = Cond (fac' @< floatF 1) (bkgDark @* fac') (bkgDark @+ (bkgLight @- bkgDark) @* (fac' @- floatF 1))
        frg = pack' (V4 (floatF 0.15 @+ surf @* floatF 0.25) (floatF 0.4 @+ surf @* floatF 0.6) (floatF 0) (floatF 0))
        smp = frg @* inf' @+ bkg @* (floatF 1 @- inf')
        V4 su sv _ _ = unpack' (fract' uv')
        surf = vignette @* max' (floatF 0) (max' diagLeft (Cond (su @< floatF 1 @- sv) diagRight (floatF 0)))
        vignette = smoothstep' (floatF 0.48) (floatF 0.4) (max' (abs' (floatF 0.5 @- su)) (abs' (floatF 0.5 @- sv)))
        diagRight = floatF 1 @- abs' (su @- sv) @* floatF 6
        diagLeft = floatF 1 @- abs' (su @+ sv @- floatF 1) @* floatF 6
        inf' = smoothstep' (floatF 0.05) (floatF 0.15) inf @* floatF 0.8
        fac' = Cond (fac @< floatF 0.25) (floatF 0) (round' (fac @* floatF 10) @/ floatF 5)
        V4 inf fac _ _ = unpack' (texture' (Sampler LinearFilter Repeat tex) uv)
        tex = TextureSlot "infectionMap" (Texture2D (Float RGBA) n1)
        uv' = texture' (Sampler LinearFilter Repeat uvMap) uv
        uvMap = TextureSlot "uvMap" (Texture2D (Float RGBA) n1)

renderUV :: Exp Obj (FrameBuffer 1 V4F)
renderUV = transformMap emptyBuffer
  where
    emptyBuffer = FrameBuffer (ColorImage n1 (V4 0 0 0 1) :. ZT)
    transformMap = Accumulate fragmentCtx PassAll transformFragmentShader transformFragmentStream
    copyMap = Accumulate fragmentCtx PassAll copyFragmentShader copyFragmentStream (initMap emptyBuffer)
    initMap = Accumulate fragmentCtx PassAll initFragmentShader initFragmentStream
    fragmentCtx = AccumulationContext Nothing (ColorOp NoBlending (V4 True True True True) :. ZT)
    rasterCtx = TriangleCtx CullNone PolygonFill NoOffset LastVertex

    transformFragmentStream = Rasterize rasterCtx (quadStream (const (floatV 1)))
    copyFragmentStream = Rasterize rasterCtx (quadStream id)
    initFragmentStream = Rasterize rasterCtx (quadStream (floatV 1 @-))
    quadStream f = Transform (vertexShader f) (Fetch "quadMesh" Triangles (IV2F "position", IV2F "uv"))

    copyFactor = Uni (IFloat "copyFactor") :: Exp V Float

    vertexShader copyFactorFn attr = VertexOut point (floatV 1) ZT (Smooth uv :. ZT)
      where
        point = v3v4 (v2v3 (pos @* floatV 2 @* copyFactorFn copyFactor @- floatV 1))
        (pos, uv) = untup2 attr

    transformFragmentShader uv = FragmentOut (uvPrev @+ uvDif :. ZT)
      where
        uvPrev = texture' (Sampler LinearFilter Repeat uvMap) uv
        uvDif = pack' (V4 (rt @- lt) (dn @- up) (floatF 0) (floatF 0))
        V4 up _ _ _ = unpack' (texture' (Sampler LinearFilter Repeat tex) (uv @- pack' (V2 (floatF 0) res)))
        V4 dn _ _ _ = unpack' (texture' (Sampler LinearFilter Repeat tex) (uv @+ pack' (V2 (floatF 0) res)))
        V4 lt _ _ _ = unpack' (texture' (Sampler LinearFilter Repeat tex) (uv @- pack' (V2 res (floatF 0))))
        V4 rt _ _ _ = unpack' (texture' (Sampler LinearFilter Repeat tex) (uv @+ pack' (V2 res (floatF 0))))
        uvMap = Texture (Texture2D (Float RGBA) n1) (V2 uvMapSize uvMapSize) NoMip [PrjFrameBuffer "uvMapCopy" tix0 copyMap]
        tex = TextureSlot "infectionMap" (Texture2D (Float RGBA) n1)
        res = floatF (0.15 / uvMapSize)

    copyFragmentShader uv = FragmentOut (smp :. ZT)
      where
        smp = texture' (Sampler LinearFilter Repeat tex) uv
        tex = TextureSlot "uvMapPrev" (Texture2D (Float RGBA) n1)

    initFragmentShader uv = FragmentOut (pack' (V4 u v (floatF 0) (floatF 0)) :. ZT)
      where
        V2 u v = unpack' (uv @* floatF 15)

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
printState (I ci _ sf _) = do
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
