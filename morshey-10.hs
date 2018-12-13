
import Eemian
import Hershey
import Data.Maybe (fromMaybe,maybeToList)
import Data.Char (toUpper,ord,chr,isDigit)
import Data.List (genericLength)
import Data.List.Split
import qualified Data.Map as Map
import Control.Concurrent.MVar 
import System.IO.Unsafe
import Graphics.UI.Gtk 
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.UI.Gtk.Gdk.EventM as M
import System.Glib.UTFString (glibToString)
{-
import Foreign.ForeignPtr
import Control.Concurrent
import Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Mixer          as SDL.Mixer
import qualified Graphics.UI.SDL.Mixer.General  as SDL.Mixer.General
import qualified Graphics.UI.SDL.Mixer.Channels as SDL.Mixer.Channels
import qualified Graphics.UI.SDL.Mixer.Types    as SDL.Mixer.Types
import qualified Graphics.UI.SDL.Mixer.Samples  as SDL.Mixer.Samples

playBeeps = do
  SDL.init [InitAudio]
  SDL.Mixer.openAudio 44100 SDL.Mixer.AudioS16LSB 2 4096
  SDL.Mixer.Channels.allocateChannels 16
  SDL.Mixer.volume 50 50
  wave <- SDL.Mixer.Samples.loadWAV "beeps.wav"
  SDL.Mixer.Channels.playChannel (-1) wave 0
  threadDelay 500000
  -- SDL.delay 10000
  -- Try also using SDL.delay 1000
  touchForeignPtr wave
  return ()

-- intToDouble :: Int -> Double
-- intToDouble = fromRational . toRational
-}
int = fromIntegral

takeWhileSumLt mx sm (x:xs)
  | sm == 0      = x : takeWhileSumLt mx (x + sp)  xs
  | sm + x <= mx = x : takeWhileSumLt mx (sm + x + sp) xs
  | otherwise    = []
  where
    sp = space
takeWhileSumLt _ _ _ = []

letterXs sm (x:xs) =
  (sm,sm + x1) : letterXs (sm + x1) xs      -- (start,end)
  where
    x1 = tellLen x
letterXs _ _ = []

fsts xs = [a | (a,b) <- xs]
snds xs = [b | (a,b) <- xs]

wordXs sm (xs:xss) = 
  (xs1,xs2) : wordXs newX xss
  where
    newX = xs2 + space
    xs1 = minimum (fsts letters)
    xs2 = maximum (snds letters)
    letters = letterXs sm xs
wordXs _ _ = []

lineXys y (xss:xsss) = 
  xs1 : lineXys (y+lineSpace) xsss
  where
    xs1 = [(x,y)| x <- fsts (wordXs 0 xss)]
lineXys _ _ = []

head1 (x:xs) = x
head1 _ = []

head2 (x:xs) = x
head2 _ = (0,0)

tail2 (x:xs) = xs
tail2 _ = []

lrHands c = (head2 . head1) (verts c)
lrHands1 ch = (head2 . head1) ch

tellLen c = abs a + abs b
  where 
   (a,b) = lrHands c

cutList maxLen [] = [[]]
cutList maxLen xs = 
  fstLn : cutList maxLen (drop (length fstLn) xs)
  where
    fstLn = takeWhileSumLt maxLen 0 xs

node1 h (i,pt) = (name,pt)
  where
    name = [chr (ord 'A' + (h-1))] ++ show i
node2 (h,pts) = map (node1 h) (zip [1..] pts)
node3 pls = map node2 (zip [1..] pls)
node4 ch = map node3 ch
--[[('I',[[("A1",(188,344)),("A2",(188,407))]])]]
nodelist = lst
  where
    lst = [[(c,node3 ch) | (c,ch) <- ws] | ws <- h4]
    h4 = mtxt

map2 = Map.fromList morse1

--h = hershey1
h = hershey2
map1 = Map.fromList h
verts c = map1 Map.! c

skale = 3
xMargin = 50
yMargin = 33 + 41
lineSpace = 21 + 13
space = 16
(imgW,imgH) = (854,480)
unitMs = 150

origRight hands (x,y) = (x + (-leftHandPos), y)
  where
    (leftHandPos,rightHandPos) = hands

--move0 (x1,y1) (x2,y2) = (x1+x2,y1+y2) 
moveLst (x1,y1) pts = [(x1+x2,y1+y2)| (x2,y2) <- pts]
--scale0 k (x1,y1) = (k*x1,k*y1) 

mkTxt1 map1 chars = h4
  where
    -- [[('L',[[(407,240),(407,303)],[(407,303),
    -- (443,303)]])]]
    h4 = [[(c,[map sm1 pls | pls <- ch]) | (c,ch) <- ws]
      | ws <- h3]
    sm1 (x,y) = 
      (skale * x + xMargin, skale * y + yMargin)
    -- ('M',[[(61,56),(61,77)],[(61,56),(69,77)],
    -- [(77,56),(69,77)],[(77,56),(77,77)]])
    h3 = [[(c, mv2 (mv1 (x,y) ch) ch) 
      | (c,(x,y),ch) <- ws] | ws <- h2]
    mv1 (x,y) ch = origRight (lrHands1 ch) (x,y)
    mv2 (x,y) ch = map (moveLst (x,y)) (tail ch)
    -- [[('L',(115,68),[[(-10,7)],[(-6,-12),(-6,9)],
    -- [(-6,9),(6,9)]])]]
    h2 = [[(c,(x+x1,y),verts c) 
      | (c,x1) <- zip cx (fsts (letterXs 0 cx))] 
        | ((x,y),cx) <- h1]
    -- [((0,0),"BON"),((81,0),"DIE!")]
    h1 = zip (concat lx) breaked 
    lx = lineXys 0 cd
    cd = splitPlaces [length x | x <- d1] cxx
    d1 = cutList maxLen ls1
    maxLen = (imgW - (2*xMargin)) `div` skale
    ls1 = [sum [tellLen c | c <- cx] | cx <- cxx]
    cxx = [[c | c <- bx] | bx <- breaked]
    breaked = words chars 

mtxt = mkTxt1 map1 text
  where
    text = map toUpper
      --"Ä ja Ö on helppo lisätä: AaOo ÄäÖö." 
      --"Helsingistä Pariisiin on 1907 kilometriä."
      --"Bon die! Hic es Alec e Adam. Illes es amicos. "
      --"Lämpötila kohoaa syvemmälle mentäessä."
      "abc, def. ghi! jkl? mno; pq-rs: t u v w x y z ä ö"

main = do
  initGUI
  var2 <- newMVar (0,0)
  var <- newMVar (1.0,0.0,0.0)
  vPos <- newMVar (None,0.0,0.0)
  window <- windowNew
  canvas <- drawingAreaNew
  surf <- return $ unsafeLoadPNG "tausta-854x480px.png"
  widgetAddEvents canvas [Button1MotionMask] 
  widgetSetSizeRequest canvas (int imgW) (int imgH)
  centerImg var surf canvas 
  strokeText surf mtxt
  -- forkOS $ playBeeps
  canvas `on` motionNotifyEvent $ do
    (mouseX,mouseY) <- eventCoordinates
    t <- M.eventTime
    C.liftIO $ changePos vPos var surf canvas mouseX mouseY
    C.liftIO $ logMsg 0 ("Motion Time: " ++ s t)
    return False
  window `on` keyPressEvent $ tryEvent $ do
    key <- eventKeyName
    keyInput var var2 surf canvas (glibToString key)
    C.liftIO $ updateCanvas1 var canvas surf
    return ()
  canvas `on` buttonPressEvent $ tryEvent $ do
    (mouseX,mouseY) <- printMouse
    C.liftIO $ printPointer canvas
    C.liftIO $ printMVar var mouseX mouseY
    C.liftIO $ modifyMVar_ vPos (\_ -> 
      return (Press,mouseX,mouseY)) 
  canvas `on` buttonReleaseEvent $ tryEvent $ do
    (mouseX,mouseY) <- M.eventCoordinates
    m <- M.eventModifier
    b <- M.eventButton
    (cause,vPosX,vPosY) <- C.liftIO $ readMVar vPos
    C.liftIO $ release cause b var vPosX vPosY
  canvas `on` scrollEvent $ tryEvent $ do
    (mouseX,mouseY) <- M.eventCoordinates
    m <- M.eventModifier
    d <- M.eventScrollDirection
    t <- M.eventTime
    C.liftIO $ changeRef var d mouseX mouseY
    C.liftIO $ updateCanvas1 var canvas surf
    C.liftIO $ logMsg 0 ("Scroll: " ++ s t ++ s mouseX ++
      s mouseY ++ s m ++ s d) 
  onDestroy window mainQuit
  onExpose canvas $ const (updateCanvas1 var canvas surf)
  timer <- timeoutAdd 
    (onTimer var var2 nodelist surf canvas) (10 * unitMs)
  set window [containerChild := canvas]
  widgetShowAll window
  mainGUI

data EvtType = Press | Release | Move | Scroll | None

release Press button var mouseX mouseY = do
  (varS,varX,varY) <- readMVar var
  let
    x = (mouseX - varX) / varS
    y = (mouseY - varY) / varS
  C.liftIO $ logMsg 0 
    ("Add point: " ++ s x ++ s y ++ s button)
  C.liftIO $ logMsg 1 (s x ++ s y)

release _ button var x y = do
  C.liftIO $ logMsg 0 ("Ignore: " ++ s x ++ s y)

changePos vPos var surf canvas mouseX mouseY = do
  (cause,vPosX,vPosY) <- readMVar vPos
  (scaleOld,oldX,oldY) <- readMVar var
  let
    dx = vPosX - mouseX
    dy = vPosY - mouseY
  modifyMVar_ var (\_ -> 
    return (scaleOld,oldX-dx,oldY-dy))
  modifyMVar_ vPos (\_ -> return (Move,mouseX,mouseY))
  updateCanvas1 var canvas surf

s x = show x ++ " "

printMouse = do
  (mouseX,mouseY) <- M.eventCoordinates
  C.liftIO $ logMsg 0 ("Mouse: " ++ s mouseX ++ s mouseY)
  return  (mouseX,mouseY)

printPointer canvas = do
  (widX,widY) <- widgetGetPointer canvas
  logMsg 0 ("Widget: " ++ s widX ++ s widY)

printMVar var mouseX mouseY = do
  (varS,varX,varY) <- readMVar var
  let
    x = (mouseX - varX) / varS
    y = (mouseY - varY) / varS
  logMsg 0 ("MVar: " ++ s varS ++ s varX ++ s varY)
  logMsg 0 ("Calc: " ++ s x ++ s y)

centerImg var surf canvas = do
  w1 <- C.imageSurfaceGetWidth surf
  h1 <- C.imageSurfaceGetHeight surf
  (w2,h2) <- widgetGetSizeRequest canvas
  let
    dh = intToDouble (h2 - h1)
    dw = intToDouble (w2 - w1)
  modifyMVar_ var (\_ -> return (1.0,dw/2,dh/2))

strokePolyLine img pts = do
  C.renderWith img ( do
    setColor Gray60
    C.setLineWidth 3.2
    C.setLineCap C.LineCapRound
    (\(x1,y1) -> C.moveTo (d x1) (d y1)) (head2 pts)
    mapM_ (\(x,y) -> C.lineTo (d x) (d y)) (tail pts)
    C.stroke
    )
  where
    d = (intToDouble . fromIntegral)

setColor color = do
  C.setSourceRGB r g b
  where
    RGB r g b = colorRGB color

strokeChar surf char = mapM_ (strokePolyLine surf) char

strokeWord surf txt = 
  mapM_ (\(c,ch) -> strokeChar surf ch) txt

strokeText surf mtxt = mapM_ (strokeWord surf) mtxt
    -- ('M',[[(61,56),(61,77)],[(61,56),(69,77)],
    -- [(77,56),(69,77)],[(77,56),(77,77)]]) 

avg2 [] = []
avg2 pts = [(x1, y1)]
  where
    x1 = sum xs `div` n
    y1 = sum ys `div` n
    xs = fsts pts
    ys = snds pts
    n = genericLength pts

nextLineSpace x k
  | z < x = nextLineSpace x (k+1)
  | otherwise = z
  where
    z = k * skale * lineSpace + 4

cornerSE pts = (x1,y2)
  where
    x1 = maximum xs
    y2 = nextLineSpace y1 0
    y1 = maximum ys
    xs = fsts pts
    ys = snds pts

mittelXY pts = (x3,y4)
  where
    y4 = nextLineSpace y3 0
    x3 = fromIntegral ((x1+x2) `div` 2)
    y3 = fromIntegral ((y1+y2) `div` 2)
    x1 = minimum xs
    x2 = maximum xs
    y1 = minimum ys
    y2 = maximum ys
    xs = fsts pts
    ys = snds pts

find1 mapA x = fromMaybe [] value
  where
    value = Map.lookup x mapA

find2 mapA x = maybeToList value
  where
    value = Map.lookup x mapA

find0 mapA x = mapA Map.! x

-- A1^4 = A1A1A1A1
replExp xs = (b1,b5)
  where
    b5 = if (null b4) then 1 else (read b4 :: Int)
    b4 = takeWhile (isDigit) b3
    b3 = drop 1 b2
    b2 = dropWhile (/='^') xs
    b1 = takeWhile (/='^') xs

comb ((c,n):xs) = replicate n c ++ comb xs
comb _ = []
  
find3 mapA xs = a
  where
    a = avg2 b3
    b3 = concat [find2 mapA x | x <- b4]
    b4 = comb [replExp x | x <- b2]
    b2 = filter (/= "") b1
    b1 = split (keepDelimsL $ oneOf ['A'..'Z']) xs

morseNext2 vCh vSt morseNodes textNodes
  | vSt >= lSt && vCh < lCh = (vCh + 1, 0)
  | vSt >= lSt && vCh >= lCh = (0,0)
  | otherwise = (vCh, vSt + 1)
  where
    lSt = length morseNodes - 1
    lCh = length textNodes - 1

nth xs n 
  | len < n = []
  | otherwise = xs !! n
  where
    len = length xs - 1

nth2 xs n 
  | len < n = (' ',[])
  | otherwise = xs !! n
  where
    len = length xs - 1

morseNext1 vCh vSt textNodes = (newCh,newSt,nd1,morse1)
  where
    morse1 = morseNodes `nth` vSt
    morseNodes = map2 `find1` letter1
    txtNds = concat textNodes
    nd1 = txtNds `nth2` vCh
    letter1 = fst nd1
    (newCh,newSt) = 
      morseNext2 vCh vSt morseNodes txtNds

underScoreMap nds = 
  [("_" ++ show n,(x1+15*(5-n),y1+10)) | n <- [1..11]]
  where
    (x1,y1) = mittelXY (snds nds)

nds1 nd1 = nd2 ++ underScoreMap nd2 -- ++ coordMap nd2
  where
    nd2 = concat (snd nd1)

nodeListFromEdgeList nd1 morse1 = 
  concat [map3 `find3` p | p <- path]
  where
    map3 = Map.fromList nd1
    path = splitOn "-" morse1

strokeMorseDash img pts = do
  C.renderWith img ( do
    setColor Orange
    C.setLineWidth 8
    C.setLineCap C.LineCapRound
    (\(x1,y1) -> C.moveTo (d x1) (d y1)) (head2 pts)
    mapM_ (\(x,y) -> C.lineTo (d x) (d y)) (tail pts)
    C.stroke
    )
 where
    d = (intToDouble . fromIntegral)

strokeMorseDot img pt = do
  C.renderWith img ( do
    setColor Orange
    C.setLineWidth 12
    C.setLineCap C.LineCapRound
    C.moveTo (d x) (d y)
    C.lineTo (d x) (d y)
    C.stroke
    )
 where
    (x,y) = pt
    d = (intToDouble . fromIntegral)

dotOrDash pts 
  | length pts == 0 = 5
  | length pts == 1 = 2
  | otherwise = 4

strokePts img pts 
  | length pts == 0 = (do logMsg 1 "Stroking empty pts!")
  | length pts == 1 = (do strokeMorseDot img (pts !! 0))
  | otherwise = (do strokeMorseDash img pts)

morseNext var var2 nodelist surf canv = do
  (vCh,vSt) <- readMVar var2
  let
    (newCh,newSt,nd1,morse1) = morseNext1 vCh vSt nodelist
    nds = nds1 nd1
    pts = nodeListFromEdgeList nds morse1 
  strokePts surf pts
  modifyMVar_ var2 (\_ -> return (newCh,newSt))
  timer <- timeoutAdd 
    (onTimer var var2 nodelist surf canv) 
    ((dotOrDash pts) * unitMs)
  logMsg 0 ("nds: " ++ show nds)
  logMsg 0 ("Length nodelist: " ++ show (length nodelist))
  logMsg 1 ("morse1: " ++ show morse1)
  logMsg 1 ("(newCh,newSt): " ++ s newCh ++ s newSt)
  logMsg 1 ("pts: " ++ show pts)

onTimer var var2 nodelist surf canv = do
  C.liftIO $ morseNext var var2 nodelist surf canv
  C.liftIO $ updateCanvas1 var canv surf
  return False -- continue loop

keyInput var var2 surf canvas key = do
  C.liftIO $ logMsg 1 ("Key Input: " ++ key)
  case key of
    x | x `elem` ["q","Escape"] -> do
      C.liftIO $ mainQuit
    "1" -> do
      C.liftIO $ centerImg var surf canvas
    "m" -> do
      C.liftIO $ putStrLn (show mtxt)
    "n" -> do
      C.liftIO $ putStrLn (show nodelist)
    "p" -> do
      C.liftIO $ strokeText surf mtxt
    "s" -> do
      C.liftIO $ createPNG surf "test-4.png"
    "space" ->
      C.liftIO $ morseNext var var2 nodelist surf canvas

changeRef var d mouseX mouseY = do
  (scaleOld,oldX,oldY) <- readMVar var
  let
    scaleD = scale1 d
    scaleNew = scaleD * scaleOld
    dx = (mouseX - oldX) * (scaleD - 1)
    dy = (mouseY - oldY) * (scaleD - 1)
    newX = oldX - dx
    newY = oldY - dy
    result =  (scaleNew,newX,newY)
  modifyMVar_ var (\_ -> return result)
  logMsg 0 ("Change MVar: " ++ s scaleNew ++ 
    s newX ++ s newY)
  where
    factor = 5/4
    scale1 ScrollUp   = factor
    scale1 ScrollDown = 1/factor

updateCanvas1 var canvas surf = do
  win <- widgetGetDrawWindow canvas
  (width, height) <- widgetGetSize canvas
  renderWithDrawable win $
    paintImage1 var surf
  return True

createPNG img fileName = do
  C.surfaceWriteToPNG img fileName
  C.liftIO ( do 
    logMsg 1 "Save Image"
    logMsg 1 ("Created: " ++ fileName)
    logMsg 1 ("Size: " ++ show imgW ++ "x" ++ show imgH ++ " pixels")
    )

imageSurfaceCreateFromPNG :: FilePath -> IO C.Surface
imageSurfaceCreateFromPNG file =
  C.withImageSurfaceFromPNG file $ \png -> do
    C.liftIO $ logMsg 0 "Load Image"
    w <- C.renderWith png $ C.imageSurfaceGetWidth png
    h <- C.renderWith png $ C.imageSurfaceGetHeight png
    surf <- C.createImageSurface C.FormatRGB24 w h
    C.renderWith surf $ do
      C.setSourceSurface png 0 0
      C.paint
    return surf

unsafeLoadPNG file = 
  unsafePerformIO $ imageSurfaceCreateFromPNG file

paintImage1 var surf = do
  (sc,x,y) <- C.liftIO $ readMVar var
  C.setSourceRGB 1 1 1
  C.paint
  C.translate x y
  C.scale sc sc
  C.liftIO $ logMsg 0 
    ("Paint Image: " ++ s sc ++ s x ++ s y)
  C.setSourceSurface surf 0 0
  C.paint

logMsg 0 s = do
  return ()
logMsg 1 s = do
  putStrLn s
  return ()

