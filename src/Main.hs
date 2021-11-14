
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, getStdGen, randomR)
import EngineModule (Game (Frogger, Snake, GameOver, Menu), displayMessage, screenGreen, getRandomNumberInRange, height, width)
import qualified Frogger (froggerMain, gamePic, move, next, startGame)
import qualified Snake (snakeMain, gamePic, move, next, startGame)
--import qualified Snake

games :: [String]
games = ["snake", "frogger"]

chooseGame :: Int -> (Int, StdGen) -> Game
chooseGame g r
    | g == 0    = Snake.startGame r
    | g == 1    = Frogger.startGame r
    | otherwise = startGame r

startGame :: (Int, StdGen) -> Game
startGame = Menu 0 

gamePic :: Game -> Picture
gamePic (Menu g r) = displayMessage $ games !! g
gamePic (Frogger p l r) = Frogger.gamePic $ Frogger p l r
gamePic (Snake s d a r) = Snake.gamePic $ Snake s d a r
gamePic (GameOver r s)  = displayMessage $ "Score: " ++ show s

positiveMod :: Int -> Int -> Int
positiveMod i n = ((i `mod` n) + n) `mod` n 

move :: Event -> Game -> Game
move (EventKey (SpecialKey KeyLeft) Down _ _) (Menu g r)    = Menu (positiveMod (g-1) 2) r
move (EventKey (SpecialKey KeyRight) Down _ _) (Menu g r)   = Menu (positiveMod (g+1) 2) r
move (EventKey (SpecialKey KeyEnter ) Down _ _) (Menu g r)  = chooseGame g $ getRandomNumberInRange (snd r) 0 $ height*width-1
move e (Frogger p l r)                                      = Frogger.move e $ Frogger p l r
move e (Snake s d a r)                                      = Snake.move e $ Snake s d a r
move (EventKey (SpecialKey KeyF5 ) Down _ _) (GameOver r s) = startGame r 
move _ g                                                    = g

next :: Float -> Game -> Game
next t (Menu g r) = Menu g r
next t (Frogger p l r) = Frogger.next t $ Frogger p l r
next t (Snake s d a r) = Snake.next t $ Snake s d a r
next t g               = g

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
-- main = Snake.snakeMain
-- main = Frogger.froggerMain