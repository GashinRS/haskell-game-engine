{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Tanks (
    tanksMain,
    gamePic,
    move,
    next,
    startGame
) where


import Data.List (delete, intersect)
import Graphics.Gloss
    ( Picture(Pictures), play, Display(InWindow) )
import Graphics.Gloss.Interface.Pure.Game
    ( Key(SpecialKey),
      KeyState(Down),
      SpecialKey(KeyF5, KeyLeft, KeyRight, KeyDown, KeyUp, KeySpace),
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
      allDirections,
      northEast,
      northWest,
      southEast,
      southWest,
      Game(Tanks, GameOver), Coord, tuplesSum )

tankDirections :: [Direction]
tankDirections = [north, east, south, west]

test :: Coord -> [Coord]
test t = fmap (tuplesSum t) allDirections

getTankCoordinates :: Coord -> Direction -> [Coord]
getTankCoordinates t d = fmap (tuplesSum t) (removeWindDirections d) ++ [t] ++ [tankHead t d]

removeWindDirections :: Direction -> [Direction]
removeWindDirections d
    | d == north = delete northWest $ delete northEast allDirections
    | d == east  = delete northEast $ delete southEast allDirections
    | d == south = delete southEast $ delete southWest allDirections
    | d == west  = delete northWest $ delete southWest allDirections

getEnemyTankCoordinates :: [Coord] -> [Direction] -> [Coord]
getEnemyTankCoordinates cs ds = concat [getTankCoordinates (cs !! t) $ ds !! t | t <- [0..length cs-1]]

-- Coordinaat van het hoofd van de tank
tankHead :: Coord -> Direction -> Coord
tankHead = tuplesSum

-- Schiet een kogel in de gegeven richting
shootBullet :: Coord -> Direction -> Coord
shootBullet t d = tuplesSum (tankHead t d) d

-- noTankCollision :: [Coord] -> [Coord] -> Bool
-- noTankCollision t ts = null $ s `intersect` concat [getTankCoordinates t' | t' <- ts]


moveTank :: Coord -> Direction -> Coord
moveTank t d
    | let n = tuplesSum t d
          tc = getTankCoordinates n d, all isInBounds tc = n
    | otherwise                                                      = t

moveTanks :: [Coord] -> [Direction] -> [Coord]
moveTanks ts ds = [moveTank (ts !! x) (ds !! x) | x <- [0..length ts - 1]]

randomDirection :: StdGen -> Direction 
randomDirection stdGen = 
    let r = getRandomNumberInRange stdGen 0 $ length tankDirections - 1 in
        tankDirections !! fst r

replaceN :: [a] -> Int -> a -> [a]
replaceN l 0 a = a:tail l
replaceN l n a = take(n-1) l ++ [a] ++ drop n l

startGame :: (Int, StdGen) -> Game
startGame = Tanks (0, bottom + 1) north [(-3, 6), (3, 6)] [south, south] [] []

gamePic :: Game -> Picture
gamePic (Tanks p d e ed b bd r) =
    Pictures[emptyBoard, Pictures[drawCoord x | x <- getTankCoordinates p d], Pictures[drawCoord x | x <- getEnemyTankCoordinates e ed],
    Pictures[drawCoord x | x <- b]]

move :: Event -> Game -> Game
move (EventKey (SpecialKey KeyLeft) Down _ _) (Tanks p d e ed b bd r)   = Tanks (moveTank p west) west e ed b bd r
move (EventKey (SpecialKey KeyRight) Down _ _) (Tanks p d e ed b bd r)  = Tanks (moveTank p east) east e ed b bd r
move (EventKey (SpecialKey KeyDown) Down _ _) (Tanks p d e ed b bd r)   = Tanks (moveTank p south) south e ed b bd r
move (EventKey (SpecialKey KeyUp) Down _ _) (Tanks p d e ed b bd r)     = Tanks (moveTank p north) north e ed b bd r
move (EventKey (SpecialKey KeySpace ) Down _ _) (Tanks p d e ed b bd r) = Tanks p d e ed (b ++ [shootBullet p d]) (bd ++ [d]) r
move (EventKey (SpecialKey KeyF5 ) Down _ _) (GameOver r s)             = GameOver r s
move _ g                                                                = g

next :: Float -> Game -> Game
next f (Tanks p d e ed b bd r) = let r' = getRandomNumberInRange (snd r) 0 $ length e in
    Tanks p d (moveTanks e ed) (replaceN ed (fst r') $ randomDirection $ snd r') [tuplesSum (b !! x) $ bd !! x | x <- [0..length b-1]] bd r'
next f g = g

tanksMain :: IO ()
tanksMain  = do
        stdGen <- getStdGen
        let r = getRandomNumberInRange stdGen 0 $ height*width-1
        play (InWindow "UGent Tanks" (500, 800) (10, 10))
             screenGreen -- de achtergrondkleur
             2 -- aantal stappen per seconde
             (startGame r) -- de beginwereld
             gamePic -- de 'render'-functie, om naar scherm te tekenen
             move -- de 'handle'-functie, om gebruiksinvoer te verwerken
             next -- de 'step'-functie, om 1 tijdstap te laten passeren