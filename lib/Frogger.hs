
--Ik ben begonnen aan Frogger maar heb dit niet afgewerkt, in de plaats daarvan heb ik Tanks en Snake gemaakt




-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- module Frogger (
--     froggerMain,
--     gamePic,
--     move,
--     next,
--     startGame
-- ) where


-- import Data.List
-- import Graphics.Gloss
-- import Graphics.Gloss.Interface.Pure.Game
--     ( Key(SpecialKey),
--       KeyState(Down),
--       SpecialKey(KeyF5, KeyLeft, KeyRight, KeyDown, KeyUp),
--       Event(EventKey) )
-- import Text.ParserCombinators.ReadPrec (prec)
-- import System.Random (StdGen, getStdGen, randomR)
-- import EngineModule
--     ( Direction,
--       Coord,
--       getRandomNumberInRange,
--       screenGreen,
--       width,
--       height,
--       north,
--       east,
--       south,
--       west,
--       bottom,
--       top,
--       left,
--       right,
--       emptyBoard,
--       drawCoord,
--       displayMessage,
--       tuplesSum,
--       moveObject,
--       isInBounds,
--       drawColored,
--       Game(Frogger, GameOver), Coord, tuplesSum )

-- maxScore :: Int 
-- maxScore = 100

-- calculateScore :: Int -> Int
-- calculateScore s 
--   | s > maxScore   = 0
--   | otherwise = maxScore - s

-- -- het aantal blokjes aan auto's en weg dat per keer gegenereerd wordt
-- streamLength :: Int
-- streamLength = 20

-- calculateStream :: StdGen -> Int -> Int -> [Coord] -> [Coord]
-- calculateStream stdGen r1 r2 c
--   | length c < streamLength = calculateStream (snd r) r1 r2 $ c ++ []
--       where r = getRandomNumberInRange stdGen 0 4

-- startGame :: (Int, StdGen) -> Game
-- startGame = Frogger (0, bottom) [[(0,0)]] 0

-- gamePic :: Game -> Picture
-- gamePic (Frogger p c t r) = Pictures[emptyBoard, Pictures[drawCoord x | x <- head c], drawColored p]
-- --gamePic (GameOver r s) = displayMessage $ "Score: " ++ show s

-- -- Neemt een coordinaat en een richting en beweegt het coordinaat in de gegeven richting.
-- -- Als het resultaat op het bord ligt wordt dit teruggegeven, anders wordt het oude coordinaat teruggegeven
-- moveInBounds :: Coord -> Direction -> Coord 
-- moveInBounds o d
--   | let n = tuplesSum o d, isInBounds n = n
--   | otherwise    = o

-- atTop :: Coord -> Bool 
-- atTop c 
--   | snd c == top = True
--   | otherwise    = False 

-- -- F5 wordt gebruikt om het spel opnieuw te starten
-- move :: Event -> Game -> Game
-- move (EventKey (SpecialKey KeyLeft) Down _ _) (Frogger p c t r)  = Frogger (moveInBounds p west) c t r
-- move (EventKey (SpecialKey KeyRight) Down _ _) (Frogger p c t r) = Frogger (moveInBounds p east) c t r
-- move (EventKey (SpecialKey KeyDown) Down _ _) (Frogger p c t r)  = Frogger (moveInBounds p south) c t r
-- move (EventKey (SpecialKey KeyUp) Down _ _) (Frogger p c t r)    = Frogger (moveInBounds p north) c t r
-- move _ g                                                         = g

-- next :: Float -> Game -> Game
-- next f (Frogger p c t r)
--   | atTop p            = GameOver r $ calculateScore t
--   | otherwise          = Frogger p c (t+1) r
-- next f (GameOver r s)  = GameOver r s

-- froggerMain :: IO ()
-- froggerMain  = do
--         stdGen <- getStdGen
--         let r = getRandomNumberInRange stdGen 0 $ height*width-1
--         play (InWindow "UGent Frogger" (500, 800) (10, 10))
--              screenGreen -- de achtergrondkleur
--              2 -- aantal stappen per seconde
--              (startGame r) -- de beginwereld
--              gamePic -- de 'render'-functie, om naar scherm te tekenen
--              move -- de 'handle'-functie, om gebruiksinvoer te verwerken
--              next -- de 'step'-functie, om 1 tijdstap te laten passeren