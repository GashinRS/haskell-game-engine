{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, getStdGen, randomR)
import EngineModule (Game (Tanks, Snake, GameOver, Menu), displayMessage, screenGreen, getRandomNumberInRange, height, width)
import qualified Snake (gamePic, move, next, startGame)
import qualified Tanks (gamePic, move, next, startGame)

games :: [String]
games = ["<-snake->", "<-tanks->"]

chooseGame :: Int -> (Int, StdGen) -> Game
chooseGame g r
    | g == 0    = Snake.startGame r
    | g == 1    = Tanks.startGame r
    | otherwise = startGame r

startGame :: (Int, StdGen) -> Game
startGame = Menu 0 

gamePic :: Game -> Picture
gamePic (Menu g r) = displayMessage $ games !! g
gamePic (Tanks p e b r) = Tanks.gamePic $ Tanks p e b r
gamePic (Snake s d a r) = Snake.gamePic $ Snake s d a r
gamePic (GameOver r s)  = displayMessage $ "Score: " ++ show s

positiveMod :: Int -> Int -> Int
positiveMod i n = ((i `mod` n) + n) `mod` n 

move :: Event -> Game -> Game
move (EventKey (SpecialKey KeyLeft) Down _ _) (Menu g r)    = Menu (positiveMod (g-1) 2) r
move (EventKey (SpecialKey KeyRight) Down _ _) (Menu g r)   = Menu (positiveMod (g+1) 2) r
move (EventKey (SpecialKey KeyEnter ) Down _ _) (Menu g r)  = chooseGame g $ getRandomNumberInRange (snd r) 0 $ height*width-1
move x (Tanks p e b r)                                      = Tanks.move x $ Tanks p e b r
move x (Snake s d a r)                                      = Snake.move x $ Snake s d a r
move (EventKey (SpecialKey KeyF5 ) Down _ _) (GameOver r s) = startGame r 
move _ g                                                    = g

next :: Float -> Game -> Game
next x (Menu g r) = Menu g r
next x (Tanks p e b r) = Tanks.next x $ Tanks p e b r
next x (Snake s d a r) = Snake.next x $ Snake s d a r
next x g               = g

main :: IO ()
main  = do 
    stdGen <- getStdGen
    let r = getRandomNumberInRange stdGen 0 $ height*width-1
    play (InWindow "UGent Brick Game" (500, 800) (10, 10))
             screenGreen -- de achtergrondkleur
             2 -- aantal stappen per seconde
             (startGame r)-- de beginwereld
             gamePic -- de 'render'-functie, om naar scherm te tekenen
             move -- de 'handle'-functie, om gebruiksinvoer te verwerken
             next -- de 'step'-functie, om 1 tijdstap te laten passeren