module Snake (
  snakeMain
) where

import Data.List
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Text.ParserCombinators.ReadPrec (prec)
import System.Random (StdGen, getStdGen, randomR)
import EngineModule

data Game
  = Playing { player :: [Coord],
              direction :: Direction,
              apple :: Coord,
              random :: (Int, StdGen)
            }
  | GameOver {
              random :: (Int, StdGen),
              score :: Int
            }

gamePic :: Game -> Picture
gamePic (Playing p d a r) = Pictures[emptyBoard, Pictures[drawCoord x | x <- p], drawCoord a]
gamePic (GameOver r s) = displayMessage $ "Score: " ++ show s

-- Geeft alle mogelijke coordinaten terug van het bord
getBoardCoordinates :: [Coord]
getBoardCoordinates = [(x, y) | x <- [left..right],
                                y <- [bottom..top]]

-- Geeft een lijst terug met de mogelijke coordinaten waar een appel terecht kan komen
getPossibleAppleLocations :: [Coord] -> [Coord]
getPossibleAppleLocations p = getBoardCoordinates \\ p

-- Geeft de game terug waarin het coordinaat van de appel werd geupdate alsook de random generator
getNewAppleLocation :: Game -> Game
getNewAppleLocation (Playing p d a r) = Playing p d a' r'
                                          where
                                            possible = getPossibleAppleLocations p
                                            r' = getRandomNumberInRange (snd r) 0 $ length possible
                                            a' = possible !! fst r'
getNewAppleLocation (GameOver r s) = GameOver r s

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
  | d == north = south
  | d == south = north
  | d == west  = east
  | d == east  = west
getOppositeDirection _ = (0,0) --default waarde die normaal gezien nooit zal voorkomen

lengthenSnake :: Direction -> [Coord] -> [Coord]
lengthenSnake d p
  | tuplesSum l south `elem` p = p ++ [tuplesSum l north]
  | tuplesSum l north `elem` p = p ++ [tuplesSum l south]
  | tuplesSum l west `elem` p  = p ++ [tuplesSum l east]
  | tuplesSum l east `elem` p  = p ++ [tuplesSum l west]
  | length p == 1              = p ++ [tuplesSum l $ getOppositeDirection d]
    where l = last p
lengthenSnake d p = p --default waarde die normaal gezien nooit zal voorkomen

next :: Float -> Game -> Game
next f (Playing p d a r)
  | let newHead = tuplesSum (head p) d in not (isInBounds newHead) || newHead `elem` p = GameOver r $ length p - 1
  | a == head p                                                                        = getNewAppleLocation (Playing (lengthenSnake d p) d a r)
  | otherwise                                                                          = Playing (moveObject p d) d a r
next t (GameOver r s)                                                                  = GameOver r s

-- F5 wordt gebruikt om het spel opnieuw te starten
move :: Event -> Game -> Game
move (EventKey (SpecialKey KeyLeft) Down _ _) (Playing p d a r)  = Playing p (preventOppositeDirection west d) a r
move (EventKey (SpecialKey KeyRight) Down _ _) (Playing p d a r) = Playing p (preventOppositeDirection east d) a r
move (EventKey (SpecialKey KeyDown) Down _ _) (Playing p d a r)  = Playing p (preventOppositeDirection south d) a r
move (EventKey (SpecialKey KeyUp) Down _ _) (Playing p d a r)    = Playing p (preventOppositeDirection north d) a r
move (EventKey (SpecialKey KeyF5 ) Down _ _) (GameOver r s)      = startGame $ getRandomNumberInRange (snd r) 0 $ height*width-1
move _ g                                                         = g

startGame :: (Int, StdGen) -> Game
startGame r = Playing [(0, 0)] north (getBoardCoordinates !! fst r ) r

snakeMain :: IO ()
snakeMain  = do
        stdGen <- getStdGen
        let r = getRandomNumberInRange stdGen 0 $ height*width-1
        play (InWindow "UGent Snake" (500, 800) (10, 10))
             screenGreen -- de achtergrondkleur
             2 -- aantal stappen per seconde
             (startGame r) -- de beginwereld
             gamePic -- de 'render'-functie, om naar scherm te tekenen
             move -- de 'handle'-functie, om gebruiksinvoer te verwerken
             next -- de 'step'-functie, om 1 tijdstap te laten passeren
