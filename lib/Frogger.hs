module Frogger (
    froggerMain
) where


import Data.List
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Text.ParserCombinators.ReadPrec (prec)
import System.Random (StdGen, getStdGen, randomR)
import EngineModule

data Game = Playing { player :: Coord,
                      logs :: [Coord],
                      random :: (Int, StdGen)
                    }
            | GameOver  {
                        random :: (Int, StdGen),
                        score :: Int
                        }

startGame :: (Int, StdGen) -> Game
startGame = Playing (0, bottom) [(0,0)]

gamePic :: Game -> Picture
gamePic (Playing p l r) = Pictures[emptyBoard, Pictures[drawCoord x | x <- l], drawColored p]
gamePic (GameOver r s) = displayMessage $ "Score: " ++ show s

-- F5 wordt gebruikt om het spel opnieuw te starten
move :: Event -> Game -> Game
move (EventKey (SpecialKey KeyLeft) Down _ _) (Playing p l r)  = Playing p l r
move (EventKey (SpecialKey KeyRight) Down _ _) (Playing p l r) = Playing p l r
move (EventKey (SpecialKey KeyDown) Down _ _) (Playing p l r)  = Playing p l r
move (EventKey (SpecialKey KeyUp) Down _ _) (Playing p l r)    = Playing p l r
move (EventKey (SpecialKey KeyF5 ) Down _ _) (GameOver r s)    = startGame $ getRandomNumberInRange (snd r) 0 $ height*width-1
move _ g                                                       = g

next :: Float -> Game -> Game
next f (Playing p l r) = Playing p l r
next t (GameOver r s)  = GameOver r s

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