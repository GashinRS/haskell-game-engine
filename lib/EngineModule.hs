module EngineModule
( getRandomNumberInRange
, screenGray
, screenGreen
, X 
, Y 
, Coord 
, Direction
, width
, height
, north
, east
, south
, west
, bottom
, top
, left
, right
, filled
, emptyBoard
, adjustSize
, drawCoord)
 where

import System.Random (StdGen, getStdGen, randomR)
import Graphics.Gloss
    ( pictures,
      rectangleSolid,
      rectangleWire,
      translate,
      play,
      makeColorI,
      Display(InWindow),
      Color,
      Picture(Text, Color, Pictures, Scale) )

type X = Int
type Y = Int
type Coord = (X, Y)
type Direction = Coord

getRandomNumberInRange :: StdGen -> Int -> Int -> (Int, StdGen)
getRandomNumberInRange stdGen l u = randomR (l, u) stdGen

screenGreen, screenGray :: Color
screenGreen = makeColorI 0x6F 0x77 0x5F 0xFF
screenGray  = makeColorI 0x64 0x6F 0x5D 0XFF

width :: X
width   = 10 -- de breedte van het bord
height :: Y
height  = 20 -- de hoogte van het bord
dblock :: Int
dblock  = 12 -- de zijde van 1 blokje (inclusief marge rondom)
dwidth :: Float
dwidth  = 10 -- de zijde van 1 blokje (exclusief marge, inclusief randje)
dinner :: Float
dinner  = 7 -- de zijde van 1 blokje (enkel het zwarte stuk middenin)
fscale :: Float
fscale  = 3 -- algemene schaal van de hele tekening
north :: Direction
north = (0, 1)
east :: Direction
east = (1, 0)
south :: Direction
south = (0, -1)
west :: Direction
west = (-1, 0)

bottom, top :: Y
left, right :: X
bottom = -(height `div` 2)
top    = height `div` 2
left   = -(width `div` 2)
right  = width `div` 2

filled :: Picture
filled = Scale fscale fscale $ Pictures[rectangleWire dwidth dwidth,rectangleSolid dinner dinner]

-- Een lege pixel, gecentreerd rond de oorsprong
empty :: Picture
empty = Color screenGray filled

-- Maakt een rij van lege pixels
makeRow :: Y -> Picture
makeRow y = pictures[drawEmpty (x, y) | x <- [left..right]]

-- Een bord met enkel lege pixels, gecentreerd rond de oorsprong
emptyBoard :: Picture
emptyBoard = Pictures[makeRow y | y <- [bottom..top]]

-- Past het coordinaat van een pixel aan om het juist te kunnen translaten
adjustSize :: Int -> Float
adjustSize v = fromIntegral (round fscale * dblock * v)

-- Een gevulde/actieve pixel op de locatie aangeduid door de coördinaat.
drawCoord :: Coord -> Picture
drawCoord c = drawPicture c filled

-- Een lege pixel op de locatie aangeduid door de coördinaat.
drawEmpty :: Coord -> Picture
drawEmpty c = drawPicture c empty

-- Hulpfunctie die gebruikt wordt om zowel tegels als gevulde pixels mee te tekenen
drawPicture :: Coord -> Picture -> Picture
drawPicture c = translate (adjustSize $ fst c) (adjustSize $ snd c)