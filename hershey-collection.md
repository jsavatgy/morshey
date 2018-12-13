# Hersheyn kirjaimet

Hersheyn kirjaimet ovat kokoelma vektorikirjasimia vuodelta 1967. Se on yksi ensimmäisiä vektorimuotoisia kirjasimia. Kokoelma on vapaasti jaettavissa.

Hersheyn kirjaimet julkaisi Allen V. Hershey (1910--2004) teoksessa *Calligraphy for Computers* (Hershey 1967). Hershey oli koulutukseltaan fyysikko ja työskenteli pääosan elämästään Yhdysvaltain laivastossa.

Hersheyn kirjaimet koostuvat ruudukkoon piirretyistä suorista viivoista. Niiden alkuperäisessä käyttötarkoituksessaan viivat piirrätettiin katodisädeputkella valokuvapaperille. Valokuvapaperi siirrettiin tämän jälkeen painokoneelle. Tietokonekuvaruutuja ei tuon ajan tietokoneissa juurikaan ollut, vaan ensisijainen tulostusväline oli paperi. 

Levityksessä olevat kirjasimet koostuvat kahdesta kokoelmasta: länsimaisesta (occidental) ja itämaisesta (oriental). Länsimaisen kokoelman tiedostonniminä ovat `hersh.oc1`, `hersh.oc2`, `hersh.oc3` ja `hersh.oc4`, ja itämaisen kokoelman tiedostoniminä `hersh.or1`, `hersh.or2`, `hersh.or3` ja `hersh.or4`.

Kukin tiedosto esittää merkkien kärkipistekoordinaatit kirjainpareina seuraavaan tapaan (merkit 1--6):

```
    1  9MWRMNV RRMVV RPSTS
    2 16MWOMOV ROMSMUNUPSQ ROQSQURUUSVOV
    3 11MXVNTMRMPNOPOSPURVTVVU
    4 12MWOMOV ROMRMTNUPUSTURVOV
    5 12MWOMOV ROMUM ROQSQ ROVUV
    6  9MVOMOV ROMUM ROQSQ
  ...
```

Rivin viisi ensimmäistä merkkiä (välilyönteineen) kertoo kirjaimen järjestysluvun, kolme seuraavaa kärkipisteiden lukumäärän. Kärkipistekoordinaatit ovat suhteessa R-kirjaimen järjestyslukuun: R-kirjain on nolla, R-kirjainta edeltävät kirjaimet negatiivisia ja R-kirjaimen jälkeiset kirjaimet positiivisia koordinaatteja. Koordinaatiston nollapiste sijaitsee merkin keskellä. Ensimmäisessä koordinaattiparissa on vasemman ja oikean laidan sijainnit. Tämän jälkeen tulevat kärkipisteiden koordinaatit ($x$,$y$)-kirjainpareina muodostaen yhtenäisiä viivoja. Merkkipari `␣R` tarkoittaa kynän siirtämistä uuteen sijaintiin viivaa piirtämättä.

Hersheyn kirjainten länsimäisessa kokoelmassa on noin 1500 merkkiä. Kokoelma sisältää latinalaisen aakkoston useina erilaisina versioina, kreikkalaiset aakkoset, kyrilliset kirjaimet ja erilaisia muun muassa kartografisia ja matemaattisia symboleja. Ä- ja Ö-kirjainta kokoelmiin ei kuulu.

Itämainen kokoelma koostuu noin 800 japanilaisesta kanji-, hiragana- ja katakana-merkistä.

# Toteutus Haskell-kielellä

Käytännön kannalta helpoin toteutustapa on koota tarvittavien merkkien kärkipistekoordinaatit omaksi kirjastokseen. Teemme sen seuraavassa Haskell-kielellä:

```
import Data.Char (ord)
import Data.List (intercalate)
import Data.List.Split (splitWhen)
import qualified Data.Map as Map

fsts xs = [a | (a,b) <- xs]
snds xs = [b | (a,b) <- xs]

pairs (x:y:xs) = xy1 : pairs xs
  where
    xy1 = (ord x - ord 'R', ord y - ord 'R')
pairs _ = []

hershey xs = [[gr]] ++ splitWhen (==(-50,0)) zs
  where
    zs = pairs x3
    x3 = drop 2 x2
    gr = (leftHandPos,rightHandPos)
    [(leftHandPos,rightHandPos)] = pairs (take 2 x2)
    x2 = drop 8 xs

startsNew xs = all (`elem` "0123456789 ") ys 
  && length ys == 8
  where
    ys = take 8 xs

process (x:y:xs)
  | startsNew y = x : process (y:xs)
  | otherwise   = process ((x ++ y) : xs)
process xs = xs

show1 xy = "module Hershey\n(\n  hershey1\n)\nwhere\n\n" ++
  "hershey1 = [\n " ++ x1 ++ "\n ]"
  where
    x1 = intercalate ",\n " x2
    x2 = map show xy

cut n (x:y:xs) 
  | n > 48 && (x==')' || x==']') && y==',' && xs /= [] = 
   [x] ++ [y] ++ "\n      " ++ cut 6 xs
  | otherwise = x : cut (n+1) (y:xs)
cut n (y:xs) = [y]
cut _ _ = []

mkModuleText content = x5
  where
    ls1 = process (lines content)
    uppers = zip ['A'..'Z'] (drop 89 ls1)
    lowers = zip ['a'..'z'] (drop 166 ls1)
    numbers = zip ['0'..'9'] (drop 251 ls1)
    punctuation = zip [' ','-','.',',',':',';','!','?'] 
      (ls1 !! 250 : ls1 !! 275 : drop 261 ls1)
    letnum = uppers ++ lowers ++ numbers ++ punctuation
    map1 = Map.fromList letnum
    ltrs = fsts letnum
    cx =  [map1 Map.! c | c <- ltrs]
    xyss = map hershey cx
    xys = zip ltrs xyss
    x2 = show1 xys
    x3 = lines x2
    x4 = map (cut 0) x3
    x5 = unlines x4

main = do
  let hersheyFile = "grass/hersheys.oc"
  content <- readFile hersheyFile
  let mdt = mkModuleText content 
  putStrLn mdt
```

Funktio `mkModuleText` tulostaa nyt listan kärkipistekoordinaateista. Tallennamme listan kirjastotiedostoon `Hershey.hs`.

```
module Hershey
(
  hershey2
)
where

hershey2 = [
 ('A',[[(-9,9)],[(0,-12),(-8,9)],[(0,-12),(8,9)],[(-5,2),
      (5,2)]]),
 ('B',[[(-11,10)],[(-7,-12),(-7,9)],[(-7,-12),(2,-12),
      (5,-11),(6,-10),(7,-8),(7,-6),(6,-4),(5,-3),(2,-2)],
      [(-7,-2),(2,-2),(5,-1),(6,0),(7,2),(7,5),(6,7),
      (5,8),(2,9),(-7,9)]]),
 ('C',[[(-10,11)],[(8,-7),(7,-9),(5,-11),(3,-12),(-1,-12),
      (-3,-11),(-5,-9),(-6,-7),(-7,-4),(-7,1),(-6,4),
      (-5,6),(-3,8),(-1,9),(3,9),(5,8),(7,6),(8,4)]]),
 ('D',[[(-11,10)],[(-7,-12),(-7,9)],[(-7,-12),(0,-12),
      (3,-11),(5,-9),(6,-7),(7,-4),(7,1),(6,4),(5,6),
      (3,8),(0,9),(-7,9)]]),
 ('E',[[(-10,9)],[(-6,-12),(-6,9)],[(-6,-12),(7,-12)],
      [(-6,-2),(2,-2)],[(-6,9),(7,9)]]),
 ('F',[[(-10,8)],[(-6,-12),(-6,9)],[(-6,-12),(7,-12)],
      [(-6,-2),(2,-2)]]),
 ('G',[[(-10,11)],[(8,-7),(7,-9),(5,-11),(3,-12),(-1,-12),
      (-3,-11),(-5,-9),(-6,-7),(-7,-4),(-7,1),(-6,4),
      (-5,6),(-3,8),(-1,9),(3,9),(5,8),(7,6),(8,4),
      (8,1)],[(3,1),(8,1)]]),
 ('H',[[(-11,11)],[(-7,-12),(-7,9)],[(7,-12),(7,9)],
      [(-7,-2),(7,-2)]]),
 ('I',[[(-4,4)],[(0,-12),(0,9)]]),
 ('J',[[(-8,8)],[(4,-12),(4,4),(3,7),(2,8),(0,9),(-2,9),
      (-4,8),(-5,7),(-6,4),(-6,2)]]),
 ('K',[[(-11,10)],[(-7,-12),(-7,9)],[(7,-12),(-7,2)],
      [(-2,-3),(7,9)]]),
 ('L',[[(-10,7)],[(-6,-12),(-6,9)],[(-6,9),(6,9)]]),
 ('M',[[(-12,12)],[(-8,-12),(-8,9)],[(-8,-12),(0,9)],
      [(8,-12),(0,9)],[(8,-12),(8,9)]]),
 ('N',[[(-11,11)],[(-7,-12),(-7,9)],[(-7,-12),(7,9)],
      [(7,-12),(7,9)]]),
 ('O',[[(-11,11)],[(-2,-12),(-4,-11),(-6,-9),(-7,-7),
      (-8,-4),(-8,1),(-7,4),(-6,6),(-4,8),(-2,9),(2,9),
      (4,8),(6,6),(7,4),(8,1),(8,-4),(7,-7),(6,-9),
      (4,-11),(2,-12),(-2,-12)]]),
 ('P',[[(-11,10)],[(-7,-12),(-7,9)],[(-7,-12),(2,-12),
      (5,-11),(6,-10),(7,-8),(7,-5),(6,-3),(5,-2),(2,-1),
      (-7,-1)]]),
 ('Q',[[(-11,11)],[(-2,-12),(-4,-11),(-6,-9),(-7,-7),
      (-8,-4),(-8,1),(-7,4),(-6,6),(-4,8),(-2,9),(2,9),
      (4,8),(6,6),(7,4),(8,1),(8,-4),(7,-7),(6,-9),
      (4,-11),(2,-12),(-2,-12)],[(1,5),(7,11)]]),
 ('R',[[(-11,10)],[(-7,-12),(-7,9)],[(-7,-12),(2,-12),
      (5,-11),(6,-10),(7,-8),(7,-6),(6,-4),(5,-3),(2,-2),
      (-7,-2)],[(0,-2),(7,9)]]),
 ('S',[[(-10,10)],[(7,-9),(5,-11),(2,-12),(-2,-12),
      (-5,-11),(-7,-9),(-7,-7),(-6,-5),(-5,-4),(-3,-3),
      (3,-1),(5,0),(6,1),(7,3),(7,6),(5,8),(2,9),(-2,9),
      (-5,8),(-7,6)]]),
 ('T',[[(-8,8)],[(0,-12),(0,9)],[(-7,-12),(7,-12)]]),
 ('U',[[(-11,11)],[(-7,-12),(-7,3),(-6,6),(-4,8),(-1,9),
      (1,9),(4,8),(6,6),(7,3),(7,-12)]]),
 ('V',[[(-9,9)],[(-8,-12),(0,9)],[(8,-12),(0,9)]]),
 ('W',[[(-12,12)],[(-10,-12),(-5,9)],[(0,-12),(-5,9)],
      [(0,-12),(5,9)],[(10,-12),(5,9)]]),
 ('X',[[(-10,10)],[(-7,-12),(7,9)],[(7,-12),(-7,9)]]),
 ('Y',[[(-9,9)],[(-8,-12),(0,-2),(0,9)],[(8,-12),(0,-2)]]),
 ('Z',[[(-10,10)],[(7,-12),(-7,9)],[(-7,-12),(7,-12)],
      [(-7,9),(7,9)]]),
 ('a',[[(-9,10)],[(6,-5),(6,9)],[(6,-2),(4,-4),(2,-5),
      (-1,-5),(-3,-4),(-5,-2),(-6,1),(-6,3),(-5,6),
      (-3,8),(-1,9),(2,9),(4,8),(6,6)]]),
 ('b',[[(-10,9)],[(-6,-12),(-6,9)],[(-6,-2),(-4,-4),
      (-2,-5),(1,-5),(3,-4),(5,-2),(6,1),(6,3),(5,6),
      (3,8),(1,9),(-2,9),(-4,8),(-6,6)]]),
 ('c',[[(-9,9)],[(6,-2),(4,-4),(2,-5),(-1,-5),(-3,-4),
      (-5,-2),(-6,1),(-6,3),(-5,6),(-3,8),(-1,9),(2,9),
      (4,8),(6,6)]]),
 ('d',[[(-9,10)],[(6,-12),(6,9)],[(6,-2),(4,-4),(2,-5),
      (-1,-5),(-3,-4),(-5,-2),(-6,1),(-6,3),(-5,6),
      (-3,8),(-1,9),(2,9),(4,8),(6,6)]]),
 ('e',[[(-9,9)],[(-6,1),(6,1),(6,-1),(5,-3),(4,-4),
      (2,-5),(-1,-5),(-3,-4),(-5,-2),(-6,1),(-6,3),
      (-5,6),(-3,8),(-1,9),(2,9),(4,8),(6,6)]]),
 ('f',[[(-5,7)],[(5,-12),(3,-12),(1,-11),(0,-8),(0,9)],
      [(-3,-5),(4,-5)]]),
 ('g',[[(-9,10)],[(6,-5),(6,11),(5,14),(4,15),(2,16),
      (-1,16),(-3,15)],[(6,-2),(4,-4),(2,-5),(-1,-5),
      (-3,-4),(-5,-2),(-6,1),(-6,3),(-5,6),(-3,8),(-1,9),
      (2,9),(4,8),(6,6)]]),
 ('h',[[(-9,10)],[(-5,-12),(-5,9)],[(-5,-1),(-2,-4),
      (0,-5),(3,-5),(5,-4),(6,-1),(6,9)]]),
 ('i',[[(-4,4)],[(-1,-12),(0,-11),(1,-12),(0,-13),(-1,-12)],
      [(0,-5),(0,9)]]),
 ('j',[[(-5,5)],[(0,-12),(1,-11),(2,-12),(1,-13),(0,-12)],
      [(1,-5),(1,12),(0,15),(-2,16),(-4,16)]]),
 ('k',[[(-9,8)],[(-5,-12),(-5,9)],[(5,-5),(-5,5)],[(-1,1),
      (6,9)]]),
 ('l',[[(-4,4)],[(0,-12),(0,9)]]),
 ('m',[[(-15,15)],[(-11,-5),(-11,9)],[(-11,-1),(-8,-4),
      (-6,-5),(-3,-5),(-1,-4),(0,-1),(0,9)],[(0,-1),
      (3,-4),(5,-5),(8,-5),(10,-4),(11,-1),(11,9)]]),
 ('n',[[(-9,10)],[(-5,-5),(-5,9)],[(-5,-1),(-2,-4),
      (0,-5),(3,-5),(5,-4),(6,-1),(6,9)]]),
 ('o',[[(-9,10)],[(-1,-5),(-3,-4),(-5,-2),(-6,1),(-6,3),
      (-5,6),(-3,8),(-1,9),(2,9),(4,8),(6,6),(7,3),
      (7,1),(6,-2),(4,-4),(2,-5),(-1,-5)]]),
 ('p',[[(-10,9)],[(-6,-5),(-6,16)],[(-6,-2),(-4,-4),
      (-2,-5),(1,-5),(3,-4),(5,-2),(6,1),(6,3),(5,6),
      (3,8),(1,9),(-2,9),(-4,8),(-6,6)]]),
 ('q',[[(-9,10)],[(6,-5),(6,16)],[(6,-2),(4,-4),(2,-5),
      (-1,-5),(-3,-4),(-5,-2),(-6,1),(-6,3),(-5,6),
      (-3,8),(-1,9),(2,9),(4,8),(6,6)]]),
 ('r',[[(-7,6)],[(-3,-5),(-3,9)],[(-3,1),(-2,-2),(0,-4),
      (2,-5),(5,-5)]]),
 ('s',[[(-8,9)],[(6,-2),(5,-4),(2,-5),(-1,-5),(-4,-4),
      (-5,-2),(-4,0),(-2,1),(3,2),(5,3),(6,5),(6,6),
      (5,8),(2,9),(-1,9),(-4,8),(-5,6)]]),
 ('t',[[(-5,7)],[(0,-12),(0,5),(1,8),(3,9),(5,9)],[(-3,-5),
      (4,-5)]]),
 ('u',[[(-9,10)],[(-5,-5),(-5,5),(-4,8),(-2,9),(1,9),
      (3,8),(6,5)],[(6,-5),(6,9)]]),
 ('v',[[(-8,8)],[(-6,-5),(0,9)],[(6,-5),(0,9)]]),
 ('w',[[(-11,11)],[(-8,-5),(-4,9)],[(0,-5),(-4,9)],
      [(0,-5),(4,9)],[(8,-5),(4,9)]]),
 ('x',[[(-8,9)],[(-5,-5),(6,9)],[(6,-5),(-5,9)]]),
 ('y',[[(-8,8)],[(-6,-5),(0,9)],[(6,-5),(0,9),(-2,13),
      (-4,15),(-6,16),(-7,16)]]),
 ('z',[[(-8,9)],[(6,-5),(-5,9)],[(-5,-5),(6,-5)],[(-5,9),
      (6,9)]]),
 ('0',[[(-10,10)],[(-1,-12),(-4,-11),(-6,-8),(-7,-3),
      (-7,0),(-6,5),(-4,8),(-1,9),(1,9),(4,8),(6,5),
      (7,0),(7,-3),(6,-8),(4,-11),(1,-12),(-1,-12)]]),
 ('1',[[(-10,10)],[(-4,-8),(-2,-9),(1,-12),(1,9)]]),
 ('2',[[(-10,10)],[(-6,-7),(-6,-8),(-5,-10),(-4,-11),
      (-2,-12),(2,-12),(4,-11),(5,-10),(6,-8),(6,-6),
      (5,-4),(3,-1),(-7,9),(7,9)]]),
 ('3',[[(-10,10)],[(-5,-12),(6,-12),(0,-4),(3,-4),(5,-3),
      (6,-2),(7,1),(7,3),(6,6),(4,8),(1,9),(-2,9),(-5,8),
      (-6,7),(-7,5)]]),
 ('4',[[(-10,10)],[(3,-12),(-7,2),(8,2)],[(3,-12),(3,9)]]),
 ('5',[[(-10,10)],[(5,-12),(-5,-12),(-6,-3),(-5,-4),
      (-2,-5),(1,-5),(4,-4),(6,-2),(7,1),(7,3),(6,6),
      (4,8),(1,9),(-2,9),(-5,8),(-6,7),(-7,5)]]),
 ('6',[[(-10,10)],[(6,-9),(5,-11),(2,-12),(0,-12),(-3,-11),
      (-5,-8),(-6,-3),(-6,2),(-5,6),(-3,8),(0,9),(1,9),
      (4,8),(6,6),(7,3),(7,2),(6,-1),(4,-3),(1,-4),
      (0,-4),(-3,-3),(-5,-1),(-6,2)]]),
 ('7',[[(-10,10)],[(7,-12),(-3,9)],[(-7,-12),(7,-12)]]),
 ('8',[[(-10,10)],[(-2,-12),(-5,-11),(-6,-9),(-6,-7),
      (-5,-5),(-3,-4),(1,-3),(4,-2),(6,0),(7,2),(7,5),
      (6,7),(5,8),(2,9),(-2,9),(-5,8),(-6,7),(-7,5),
      (-7,2),(-6,0),(-4,-2),(-1,-3),(3,-4),(5,-5),(6,-7),
      (6,-9),(5,-11),(2,-12),(-2,-12)]]),
 ('9',[[(-10,10)],[(6,-5),(5,-2),(3,0),(0,1),(-1,1),
      (-4,0),(-6,-2),(-7,-5),(-7,-6),(-6,-9),(-4,-11),
      (-1,-12),(0,-12),(3,-11),(5,-9),(6,-5),(6,0),
      (5,5),(3,8),(0,9),(-2,9),(-5,8),(-6,6)]]),
 (' ',[[(-8,8)],[]]),
 ('-',[[(-13,13)],[(-9,0),(9,0)]]),
 ('.',[[(-5,5)],[(0,7),(-1,8),(0,9),(1,8),(0,7)]]),
 (',',[[(-5,5)],[(1,8),(0,9),(-1,8),(0,7),(1,8),(1,10),
      (0,12),(-1,13)]]),
 (':',[[(-5,5)],[(0,-5),(-1,-4),(0,-3),(1,-4),(0,-5)],
      [(0,7),(-1,8),(0,9),(1,8),(0,7)]]),
 (';',[[(-5,5)],[(0,-5),(-1,-4),(0,-3),(1,-4),(0,-5)],
      [(1,8),(0,9),(-1,8),(0,7),(1,8),(1,10),(0,12),
      (-1,13)]]),
 ('!',[[(-5,5)],[(0,-12),(0,2)],[(0,7),(-1,8),(0,9),
      (1,8),(0,7)]]),
 ('?',[[(-9,9)],[(-6,-7),(-6,-8),(-5,-10),(-4,-11),
      (-2,-12),(2,-12),(4,-11),(5,-10),(6,-8),(6,-6),
      (5,-4),(4,-3),(0,-1),(0,2)],[(0,7),(-1,8),(0,9),
      (1,8),(0,7)]])
 ]
```

Voimme nyt käyttää kirjastossa lueteltuja kärkipistekoordinaatteja Hersheyn kirjainten piirtämiseen pääohjelmassa.

```
import Hershey
import Data.Char (toUpper,ord,chr)
import Data.List.Split (splitPlaces)
import qualified Data.Map as Map
import Control.Concurrent.MVar 
import System.IO.Unsafe
import Graphics.UI.Gtk 
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.UI.Gtk.Gdk.EventM as M
import System.Glib.UTFString (glibToString)

intToDouble :: Int -> Double
intToDouble = fromRational . toRational

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
tgal2 _ = []

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

h = hershey2
map1 = Map.fromList h
verts c = map1 Map.! c

skale = 3
xMargin = 50
yMargin = 33 + 41
lineSpace = 21 + 13
space = 16
(imgW,imgH) = (854,480)

origRight hands (x,y) = (x + (-leftHandPos), y)
  where
    (leftHandPos,rightHandPos) = hands

move0 (x1,y1) (x2,y2) = (x1+x2,y1+y2) 
move2 (x1,y1) pts = [(x1+x2,y1+y2)| (x2,y2) <- pts]
scale0 k (x1,y1) = (k*x1,k*y1) 

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
    mv2 (x,y) ch = map (move2 (x,y)) (tail ch)
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
    text = -- map toUpper
      "Pariisista Helsinkiin on 1907 km." 
      --"Helsingistä Pariisiin on 1907 kilometriä."
      --"Bon die! Hic es Alec e Adam. Illes es amicos. "

main = do
  initGUI
  var <- newMVar (1.0,0.0,0.0)
  vPos <- newMVar (None,0.0,0.0)
  window <- windowNew
  canvas <- drawingAreaNew
  surf <- return $ unsafeLoadPNG "tausta-854x480px.png"
  widgetAddEvents canvas [Button1MotionMask] 
  widgetSetSizeRequest canvas (int imgW) (int imgH)
  centerImg var surf canvas 
  canvas `on` motionNotifyEvent $ do
    (mouseX,mouseY) <- eventCoordinates
    t <- M.eventTime
    C.liftIO $ changePos vPos var surf canvas mouseX mouseY
    C.liftIO $ logMsg 0 ("Motion Time: " ++ s t)
    return False
  window `on` keyPressEvent $ tryEvent $ do
    key <- eventKeyName
    keyInput var surf canvas (glibToString key)
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
    C.setSourceRGB 0.7 0.7 0.7
    C.setLineWidth 3.2
    C.setLineCap C.LineCapRound
    (\(x1,y1) -> C.moveTo (d x1) (d y1)) (head2 pts)
    mapM_ (\(x,y) -> C.lineTo (d x) (d y)) (tail pts)
    C.stroke
    )
  where
    d = (intToDouble . fromIntegral)

strokeChar surf char = mapM_ (strokePolyLine surf) char

strokeWord surf txt = 
  mapM_ (\(c,ch) -> strokeChar surf ch) txt

strokeText surf mtxt = mapM_ (strokeWord surf) mtxt
    -- ('M',[[(61,56),(61,77)],[(61,56),(69,77)],
    -- [(77,56),(69,77)],[(77,56),(77,77)]]) 

keyInput var surf canvas key = do
  C.liftIO $ logMsg 1 ("Key Input: " ++ key)
  case key of
    "q" -> do
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
      C.liftIO $ createPNG surf "test-2.png"

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
```

Kuvassa \ref{fig:esimkuva} on ohjelman tallentama PNG-kuva.

\begin{figure}[H]
\centering
\includegraphics[width=\textwidth]{test-2.png}
\caption{Esimerkkiohjelman tallentama PNG-muotoinen kokeilukuva.}\label{fig:esimkuva}
\end{figure}

# Ä- ja Ö-kirjaimen lisääminen

Lisätäksemme kokoelmaan Ä- ja Ö-kirjaimet, kopioimme sopivan kokoisen pisteen i-kirjaimesta:

```
 ('i',[[(-4,4)],[(-1,-12),(0,-11),(1,-12),(0,-13),(-1,-12)],
      [(0,-5),(0,9)]]),
```

Voimme siirtää sen haluamaamme paikkaan komennolla `move0`:

```
> map (move0 (-3,-3)) [(-1,-12),(0,-11),(1,-12),(0,-13),(-1,-12)]
[(-4,-15),(-3,-14),(-2,-15),(-3,-16),(-4,-15)]
> map (move0 (3,-3)) [(-1,-12),(0,-11),(1,-12),(0,-13),(-1,-12)]
[(2,-15),(3,-14),(4,-15),(3,-16),(2,-15)]
```

Muodostamme Ä-kirjaimen A-kirjaimesta:

```
 ('A',[[(-9,9)],[(0,-12),(-8,9)],[(0,-12),(8,9)],[(-5,2),
      (5,2)]])

 ('Ä',[[(-9,9)],[(0,-12),(-8,9)],[(0,-12),(8,9)],[(-5,2),
      (5,2)],
      [(-3,-15),(-2,-14),(-1,-15),(-2,-16),(-3,-15)],
      [(1,-15),(2,-14),(3,-15),(2,-16),(1,-15)]])
```

Ö-kirjaimen O-kirjaimesta:

```
 ('O',[[(-11,11)],[(-2,-12),(-4,-11),(-6,-9),(-7,-7),
      (-8,-4),(-8,1),(-7,4),(-6,6),(-4,8),(-2,9),(2,9),
      (4,8),(6,6),(7,4),(8,1),(8,-4),(7,-7),(6,-9),
      (4,-11),(2,-12),(-2,-12)]])

 ('Ö',[[(-11,11)],[(-2,-12),(-4,-11),(-6,-9),(-7,-7),
      (-8,-4),(-8,1),(-7,4),(-6,6),(-4,8),(-2,9),(2,9),
      (4,8),(6,6),(7,4),(8,1),(8,-4),(7,-7),(6,-9),
      (4,-11),(2,-12),(-2,-12)],
      [(-4,-15),(-3,-14),(-2,-15),(-3,-16),(-4,-15)],
      [(2,-15),(3,-14),(4,-15),(3,-16),(2,-15)]])
```

Pikku-ä syntyy a-kirjaimesta:

```
 ('a',[[(-9,10)],[(6,-5),(6,9)],[(6,-2),(4,-4),(2,-5),
      (-1,-5),(-3,-4),(-5,-2),(-6,1),(-6,3),(-5,6),
      (-3,8),(-1,9),(2,9),(4,8),(6,6)]])

 ('ä',[[(-9,10)],[(6,-5),(6,9)],[(6,-2),(4,-4),(2,-5),
      (-1,-5),(-3,-4),(-5,-2),(-6,1),(-6,3),(-5,6),
      (-3,8),(-1,9),(2,9),(4,8),(6,6)],
      [(-3,-10),(-2,-9),(-1,-10),(-2,-11),(-3,-10)],
      [(2,-10),(3,-9),(4,-10),(3,-11),(2,-10)]])
```

ja lopuksi pikku-ö o-kirjaimesta:

```
 ('o',[[(-9,10)],[(-1,-5),(-3,-4),(-5,-2),(-6,1),(-6,3),
      (-5,6),(-3,8),(-1,9),(2,9),(4,8),(6,6),(7,3),
      (7,1),(6,-2),(4,-4),(2,-5),(-1,-5)]])

 ('ö',[[(-9,10)],[(-1,-5),(-3,-4),(-5,-2),(-6,1),(-6,3),
      (-5,6),(-3,8),(-1,9),(2,9),(4,8),(6,6),(7,3),
      (7,1),(6,-2),(4,-4),(2,-5),(-1,-5)],
      [(-3,-10),(-2,-9),(-1,-10),(-2,-11),(-3,-10)],
      [(2,-10),(3,-9),(4,-10),(3,-11),(2,-10)]])
```

Kuvassa \ref{fig:ao-kirjaimet} voimme nähdä miltä uudet kirjaimet näyttävät valmiina.

\begin{figure}[H]
\centering
\includegraphics[width=\textwidth]{test-3.png}
\caption{Kirjaimet Ä, Ö, ä ja ö.}\label{fig:ao-kirjaimet}
\end{figure}


# Kirjainten muodostuminen viivoista

Hersheyn kirjaimet muodostuvat kärkipistekoordinaattien muodostamista viivajonoista.

Seuraavilla sivuilla esittelemme solmujonoina suuraakkoset A--Z (koodit 89--114), numerot 0--9 (koodit 251--260), pisteen (261), pilkun (262), kaksoispisteen (263), puolipisteen (264), huutomerkin (265) ja kysymysmerkin (266) (kuvat 3--44). 

Solmujonot saamme solmujonojen koordinaateista purkamalla teksti sanojen listaksi, sanojen lista kirjainhahmoiksi, kirjainhahmot viivajonoiksi ja lopuksi viivajonot kärkipistekoordinaateiksi.

```
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
```

\begin{figure}[H]
\centering
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-A.pdf}}
\caption{A-kirjain.}\end{minipage}\hfill
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-B.pdf}}
\caption{B-kirjain.}\end{minipage}
\end{figure}

\begin{figure}[H]
\centering
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-C.pdf}}
\caption{C-kirjain.}\end{minipage}\hfill
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-D.pdf}}
\caption{D-kirjain.}\end{minipage}
\end{figure}

\begin{figure}[H]
\centering
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-E.pdf}}
\caption{E-kirjain.}\end{minipage}\hfill
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-F.pdf}}
\caption{F-kirjain.}\end{minipage}
\end{figure}

\begin{figure}[H]
\centering
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-G.pdf}}
\caption{G-kirjain.}\end{minipage}\hfill
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-H.pdf}}
\caption{H-kirjain.}\end{minipage}
\end{figure}

\begin{figure}[H]
\centering
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-I.pdf}}
\caption{I-kirjain.}\end{minipage}\hfill
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-J.pdf}}
\caption{J-kirjain.}\end{minipage}
\end{figure}

\begin{figure}[H]
\centering
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-K.pdf}}
\caption{K-kirjain.}\end{minipage}\hfill
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-L.pdf}}
\caption{L-kirjain.}\end{minipage}
\end{figure}

\begin{figure}[H]
\centering
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-M.pdf}}
\caption{M-kirjain.}\end{minipage}\hfill
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-N.pdf}}
\caption{N-kirjain.}\end{minipage}
\end{figure}

\begin{figure}[H]
\centering
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-O.pdf}}
\caption{O-kirjain.}\end{minipage}\hfill
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-P.pdf}}
\caption{P-kirjain.}\end{minipage}
\end{figure}

\begin{figure}[H]
\centering
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-Q.pdf}}
\caption{Q-kirjain.}\end{minipage}\hfill
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-R.pdf}}
\caption{R-kirjain.}\end{minipage}
\end{figure}

\begin{figure}[H]
\centering
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-S.pdf}}
\caption{S-kirjain.}\end{minipage}\hfill
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-T.pdf}}
\caption{T-kirjain.}\end{minipage}
\end{figure}

\begin{figure}[H]
\centering
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-U.pdf}}
\caption{U-kirjain.}\end{minipage}\hfill
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-V.pdf}}
\caption{V-kirjain.}\end{minipage}
\end{figure}

\begin{figure}[H]
\centering
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-W.pdf}}
\caption{W-kirjain.}\end{minipage}\hfill
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-X.pdf}}
\caption{X-kirjain.}\end{minipage}
\end{figure}

\begin{figure}[H]
\centering
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-Y.pdf}}
\caption{Y-kirjain.}\end{minipage}\hfill
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-Z.pdf}}
\caption{Z-kirjain.}\end{minipage}
\end{figure}

\begin{figure}[H]
\centering
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-0.pdf}}
\caption{Numero 0.}\end{minipage}\hfill
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-1.pdf}}
\caption{Numero 1.}\end{minipage}
\end{figure}

\begin{figure}[H]
\centering
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-2.pdf}}
\caption{Numero 2.}\end{minipage}\hfill
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-3.pdf}}
\caption{Numero 3.}\end{minipage}
\end{figure}

\begin{figure}[H]
\centering
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-4.pdf}}
\caption{Numero 4.}\end{minipage}\hfill
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-5.pdf}}
\caption{Numero 5.}\end{minipage}
\end{figure}

\begin{figure}[H]
\centering
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-6.pdf}}
\caption{Numero 6.}\end{minipage}\hfill
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-7.pdf}}
\caption{Numero 7.}\end{minipage}
\end{figure}

\begin{figure}[H]
\centering
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-8.pdf}}
\caption{Numero 8.}\end{minipage}\hfill
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-9.pdf}}
\caption{Numero 9.}\end{minipage}
\end{figure}

\begin{figure}[H]
\centering
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-262.pdf}}
\caption{Piste.}\end{minipage}\hfill
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-263.pdf}}
\caption{Pilkku.}\end{minipage}
\end{figure}

\begin{figure}[H]
\centering
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-264.pdf}}
\caption{Kaksoispiste.}\end{minipage}\hfill
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-265.pdf}}
\caption{Puolipiste.}\end{minipage}
\end{figure}

\begin{figure}[H]
\centering
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-266.pdf}}
\caption{Huutomerkki.}\end{minipage}\hfill
\begin{minipage}{0.45\textwidth}
\centering
\fbox{\includegraphics{tikz/hershey-267.pdf}}
\label{hershey-questionmark}
\caption{Kysymysmerkki.}\end{minipage}
\end{figure}

# Kirjainluettelot

Seuraavassa esitämme luettelon tiedostolistauksessa mukana olevista kirjaimista (kuvat 45--68).

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz2/hershey-00.pdf}}
\label{fig:katalog-start}
\caption{Merkit 1--100.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz2/hershey-01.pdf}}
\caption{Merkit 101--200.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz2/hershey-02.pdf}}
\caption{Merkit 201--300.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz2/hershey-03.pdf}}
\caption{Merkit 301--400.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz2/hershey-04.pdf}}
\caption{Merkit 401--500.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz2/hershey-05.pdf}}
\caption{Merkit 501--600.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz2/hershey-06.pdf}}
\caption{Merkit 601--700.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz2/hershey-07.pdf}}
\caption{Merkit 701--800.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz2/hershey-08.pdf}}
\caption{Merkit 801--900.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz2/hershey-09.pdf}}
\caption{Merkit 901--1000.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz2/hershey-10.pdf}}
\caption{Merkit 1001--1100.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz2/hershey-11.pdf}}
\caption{Merkit 1101--1200.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz2/hershey-12.pdf}}
\caption{Merkit 1201--1300.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz2/hershey-13.pdf}}
\caption{Merkit 1301--1400.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz2/hershey-14.pdf}}
\caption{Merkit 1401--1500.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz2/hershey-15.pdf}}
\caption{Merkit 1501--1597.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz3/hershey-00.pdf}}
\caption{Oriental-kokoelman merkit 1--100.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz3/hershey-01.pdf}}
\caption{Oriental-kokoelman merkit 101--200.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz3/hershey-02.pdf}}
\caption{Oriental-kokoelman merkit 201--300.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz3/hershey-03.pdf}}
\caption{Oriental-kokoelman merkit 301--400.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz3/hershey-04.pdf}}
\caption{Oriental-kokoelman merkit 401--500.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz3/hershey-05.pdf}}
\caption{Oriental-kokoelman merkit 501--600.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz3/hershey-06.pdf}}
\caption{Oriental-kokoelman merkit 601--700.}\end{figure}

\begin{figure}[H]
\centering
\fbox{\includegraphics{tikz3/hershey-07.pdf}}
\label{katalog-end}
\caption{Oriental-kokoelman merkit 701--758.}\end{figure}


