import EngineModule
import Snake
import Frogger
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

newtype Game = Playing {game :: Int }

games :: [String]
games = ["snake", "frogger"]

startGame :: Game
startGame = Playing 0

gamePic :: Game -> Picture
gamePic (Playing g) = displayMessage $ games !! g

positiveMod :: Int -> Int -> Int
positiveMod i n = ((i `mod` n) + n) `mod` n 

move :: Event -> Game -> Game
move (EventKey (SpecialKey KeyLeft) Down _ _) (Playing g)   = Playing $ positiveMod (g-1) 2
move (EventKey (SpecialKey KeyRight) Down _ _) (Playing g)  = Playing $ positiveMod (g+1) 2
move (EventKey (SpecialKey KeyEnter ) Down _ _) (Playing g) = Playing g
move _ g                                                    = g

next :: Float -> Game -> Game
next f (Playing g) = Playing g

main :: IO ()
main  = play (InWindow "UGent Brick Game" (500, 800) (10, 10))
             screenGreen -- de achtergrondkleur
             2 -- aantal stappen per seconde
             startGame -- de beginwereld
             gamePic -- de 'render'-functie, om naar scherm te tekenen
             move -- de 'handle'-functie, om gebruiksinvoer te verwerken
             next -- de 'step'-functie, om 1 tijdstap te laten passeren
-- main :: IO()
-- main = froggerMain