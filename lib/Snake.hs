{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Snake (
    gamePic,
    move,
    next,
    startGame
) where

import Data.List
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
    ( KeyState(Down),
      SpecialKey(KeyF5, KeyLeft, KeyRight, KeyDown, KeyUp),
      Key(SpecialKey),
      Event(EventKey) )
import Text.ParserCombinators.ReadPrec (prec)
import System.Random (StdGen, getStdGen, randomR)
import EngineModule
    ( Game(Snake, GameOver),
      Direction,
      Coord,
      getRandomNumberInRange,
      screenGreen,
      width,
      height,
      north,
      east,
      south,
      west,
      bottom,
      top,
      left,
      right,
      emptyBoard,
      drawCoord,
      displayMessage,
      tuplesSum,
      moveObject,
      isInBounds)


-- Geeft alle mogelijke coordinaten terug van het bord
getBoardCoordinates :: [Coord]
getBoardCoordinates = [(x, y) | x <- [left..right],
                                y <- [bottom..top]]

-- Geeft een lijst terug met de mogelijke coordinaten waar een appel terecht kan komen
getPossibleAppleLocations :: [Coord] -> [Coord]
getPossibleAppleLocations p = getBoardCoordinates \\ p

-- Geeft de game terug waarin het coordinaat van de appel werd geupdate alsook de random generator
getNewAppleLocation :: Game -> Game
getNewAppleLocation (Snake p d a r) = Snake p d a' r'
                                          where
                                            possible = getPossibleAppleLocations p
                                            r'       = getRandomNumberInRange (snd r) 0 $ length possible
                                            a'       = possible !! fst r'
getNewAppleLocation (GameOver r s)  = GameOver r s

-- zorgt ervoor dat de slang niet in de omgekeerde richting kan gaan
preventOppositeDirection :: Direction -> Direction -> Direction
preventOppositeDirection n o
  | n == north && o == south = south
  | n == south && o == north = north
  | n == east && o == west   = west
  | n == west && o == east   = east
  | otherwise                = n

getOppositeDirection :: Direction -> Direction
getOppositeDirection d
  | d == north         = south
  | d == south         = north
  | d == west          = east
  | d == east          = west
getOppositeDirection _ = (0,0) --default waarde die normaal gezien nooit zal voorkomen

lengthenSnake :: Direction -> [Coord] -> [Coord]
lengthenSnake d p
  | tuplesSum l south `elem` p = p ++ [tuplesSum l north]
  | tuplesSum l north `elem` p = p ++ [tuplesSum l south]
  | tuplesSum l west `elem` p  = p ++ [tuplesSum l east]
  | tuplesSum l east `elem` p  = p ++ [tuplesSum l west]
  | length p == 1              = p ++ [tuplesSum l $ getOppositeDirection d]
    where l = last p
lengthenSnake d p              = p --default waarde die normaal gezien nooit zal voorkomen

startGame :: (Int, StdGen) -> Game
startGame r = Snake [(0, 0)] north (getBoardCoordinates !! fst r ) r

gamePic :: Game -> Picture
gamePic (Snake p d a r) = Pictures[emptyBoard, Pictures[drawCoord x | x <- p], drawCoord a]

move :: Event -> Game -> Game
move (EventKey (SpecialKey KeyLeft) Down _ _) (Snake p d a r)    = Snake p (preventOppositeDirection west d) a r
move (EventKey (SpecialKey KeyRight) Down _ _) (Snake p d a r)   = Snake p (preventOppositeDirection east d) a r
move (EventKey (SpecialKey KeyDown) Down _ _) (Snake p d a r)    = Snake p (preventOppositeDirection south d) a r
move (EventKey (SpecialKey KeyUp) Down _ _) (Snake p d a r)      = Snake p (preventOppositeDirection north d) a r
move _ g                                                         = g

next :: Float -> Game -> Game
next f (Snake p d a r)
  | let newHead = tuplesSum (head p) d in not (isInBounds newHead) || newHead `elem` p = GameOver r $ length p - 1
  | a == head p                                                                        = getNewAppleLocation (Snake (lengthenSnake d p) d a r)
  | otherwise                                                                          = Snake (moveObject p d) d a r
next t (GameOver r s)                                                                  = GameOver r s