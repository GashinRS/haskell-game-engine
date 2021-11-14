{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Frogger (
    froggerMain,
    gamePic,
    move,
    next,
    startGame
) where


import Data.List
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
    ( Key(SpecialKey),
      KeyState(Down),
      SpecialKey(KeyF5, KeyLeft, KeyRight, KeyDown, KeyUp),
      Event(EventKey) )
import Text.ParserCombinators.ReadPrec (prec)
import System.Random (StdGen, getStdGen, randomR)
import EngineModule
    ( Direction,
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
      isInBounds,
      drawColored,
      Game(Frogger, GameOver), Coord, tuplesSum )

startGame :: (Int, StdGen) -> Game
startGame = Frogger (0, bottom) [(0,0)]

gamePic :: Game -> Picture
gamePic (Frogger p l r) = Pictures[emptyBoard, Pictures[drawCoord x | x <- l], drawColored p]
--gamePic (GameOver r s) = displayMessage $ "Score: " ++ show s

-- Neemt een coordinaat en een richting en beweegt het coordinaat in de gegeven richting.
-- Als het resultaat op het bord ligt wordt dit teruggegeven, anders wordt het oude coordinaat teruggegeven
moveInBounds :: Coord -> Direction -> Coord 
moveInBounds o d
  | let n = tuplesSum o d, isInBounds n = n
  | otherwise    = o

-- F5 wordt gebruikt om het spel opnieuw te starten
move :: Event -> Game -> Game
move (EventKey (SpecialKey KeyLeft) Down _ _) (Frogger p l r)  = Frogger (moveInBounds p west) l r
move (EventKey (SpecialKey KeyRight) Down _ _) (Frogger p l r) = Frogger (moveInBounds p east) l r
move (EventKey (SpecialKey KeyDown) Down _ _) (Frogger p l r)  = Frogger (moveInBounds p south) l r
move (EventKey (SpecialKey KeyUp) Down _ _) (Frogger p l r)    = Frogger (moveInBounds p north) l r
move (EventKey (SpecialKey KeyF5 ) Down _ _) (GameOver r s)    = startGame $ getRandomNumberInRange (snd r) 0 $ height*width-1
move _ g                                                       = g

next :: Float -> Game -> Game
next f (Frogger p l r) = Frogger p l r
next f (GameOver r s)  = GameOver r s

froggerMain :: IO ()
froggerMain  = do
        stdGen <- getStdGen
        let r = getRandomNumberInRange stdGen 0 $ height*width-1
        play (InWindow "UGent Frogger" (500, 800) (10, 10))
             screenGreen -- de achtergrondkleur
             2 -- aantal stappen per seconde
             (startGame r) -- de beginwereld
             gamePic -- de 'render'-functie, om naar scherm te tekenen
             move -- de 'handle'-functie, om gebruiksinvoer te verwerken
             next -- de 'step'-functie, om 1 tijdstap te laten passeren