module EngineModule
( getRandomNumberInRange
, screenGray
, screenGreen
, X 
, Y 
, Coord 
, Direction)
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