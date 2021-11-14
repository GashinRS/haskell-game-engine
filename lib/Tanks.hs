{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Tanks (
    gamePic,
    move,
    next,
    startGame
) where


import Data.List (delete, intersect, dropWhile, takeWhile, elemIndex)
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
      Game(Tanks, GameOver),
      Coord,
      Bullet(TankBullet),
      co,
      dir,
      center,
      tdir,
      Tank(Tank),
      tuplesSum)

totalTanks :: Int 
totalTanks = 2

tankDirections :: [Direction]
tankDirections = [north, east, south, west]

getTankCoordinates :: Tank -> [Coord]
getTankCoordinates t = fmap (tuplesSum $ center t) (removeWindDirections $ tdir t) ++ [center t]

removeWindDirections :: Direction -> [Direction]
removeWindDirections d
    | d == north = delete northWest $ delete northEast allDirections
    | d == east  = delete northEast $ delete southEast allDirections
    | d == south = delete southEast $ delete southWest allDirections
    | d == west  = delete northWest $ delete southWest allDirections

getEnemyTankCoordinates :: [Tank] -> [Coord]
getEnemyTankCoordinates ts = concat [getTankCoordinates t | t <- ts]

-- Coordinaat van het hoofd van de tank
tankHead :: Coord -> Direction -> Coord
tankHead = tuplesSum

-- Laat een tank een kogel schieten
shootBullet :: Tank -> Bullet
shootBullet t = TankBullet (tuplesSum (tankHead (center t) $ tdir t) $ tdir t) $ tdir t

-- laat misschien een tank een kogel schieten
maybeShootBullet :: [Tank] -> (Int, StdGen) -> [Bullet]
maybeShootBullet ts r
    | n `mod` 3 == 0 = [shootBullet i] -- 33% kans om een kogel te schieten
    | otherwise      = []
        where n = fst $ getRandomNumberInRange (snd r) 0 30
              i = ts !! fst r

-- Checkt of een tank bots met de andere tanks op het veld
noTankCollision :: Tank -> [Tank] -> Bool
noTankCollision t ts = null $ getTankCoordinates t `intersect` concat [getTankCoordinates t' | t' <- ts]

moveTank :: Tank -> Direction -> [Tank] -> Tank
moveTank t d ts
    | let n   = Tank (tuplesSum (center t) d) d
          tc  = getTankCoordinates n
          ts' = delete t ts,
            all isInBounds tc && noTankCollision n ts' = n
    | otherwise                                        = Tank (center t) d

-- Je geeft de speler, vijanden en een random generator mee, de functie verandert willekeurig van 1 tank de richting en beweegt alle tanks
moveTanks :: Tank -> [Tank] -> (Int, StdGen) -> [Tank]
moveTanks t ts r = let ts' = changeDirection ts r in
                    [moveTank x (tdir x) (t:ts')| x <- ts']

changeDirection :: [Tank] -> (Int, StdGen) -> [Tank]
changeDirection ts r = let toBeReplaced = (ts !! fst r) in
                        Tank (center toBeReplaced) (randomDirection $ snd r) : delete toBeReplaced ts

isBulletInBounds :: Bullet -> Bool
isBulletInBounds b = isInBounds $ co b

moveBullets :: [Bullet] -> [Bullet]
moveBullets b = filter isBulletInBounds [TankBullet (tuplesSum (co x) $ dir x) (dir x) | x <- b]

randomDirection :: StdGen -> Direction
randomDirection stdGen =
    let r = getRandomNumberInRange stdGen 0 $ length tankDirections - 1 in
        tankDirections !! fst r

-- Gaat na of een tank geraakt is door een kogel
notHit :: [Bullet] -> Tank -> Bool
notHit b t = null $ getTankCoordinates t `intersect` [co x | x <- b]

removeHitTanks :: [Tank] -> [Bullet] -> [Tank]
removeHitTanks t b = filter (notHit b) t

-- Gaat na of een kogel een tank heeft geraakt
bulletHit :: [Tank] -> Bullet -> Bool
bulletHit t b = null $getEnemyTankCoordinates t `intersect` [co b]

-- Verwijdert kogels die een tank hebben geraakt
removeBullets :: [Bullet] -> [Tank] -> [Bullet]
removeBullets b t = filter (bulletHit t) b

startGame :: (Int, StdGen) -> Game
startGame = Tanks (Tank (0, bottom + 1) north) [Tank (-3, 6) south, Tank (3, 6) south] []

gamePic :: Game -> Picture
gamePic (Tanks p e b r) =
    Pictures[emptyBoard, Pictures[drawCoord x | x <- getTankCoordinates p], Pictures[drawCoord x | x <- getEnemyTankCoordinates e],
    Pictures[drawCoord (co x) | x <- b]]

move :: Event -> Game -> Game
move (EventKey (SpecialKey KeyLeft) Down _ _) (Tanks p e b r)   = Tanks (moveTank p west e) e b r
move (EventKey (SpecialKey KeyRight) Down _ _) (Tanks p e b r)  = Tanks (moveTank p east e) e b r
move (EventKey (SpecialKey KeyDown) Down _ _) (Tanks p e b r)   = Tanks (moveTank p south e) e b r
move (EventKey (SpecialKey KeyUp) Down _ _) (Tanks p e b r)     = Tanks (moveTank p north e) e b r
move (EventKey (SpecialKey KeySpace ) Down _ _) (Tanks p e b r) = Tanks p e (b ++ [shootBullet p]) r
move _ g                                                        = g

-- Tanks sterven wanneer ze worden geraakt door eender welke kogel, het maakt niet uit door wie deze werd geschoten. 
-- Dit wil zeggen dat vijandige tanks elkaar ook kunnen doden, en dat de speler zichzelf kan doden. Vijandige tanks kunnen zichzelf echter niet doden.
-- Tanks kunnen niet tegen elkaar rijden, dit wordt geblokkeerd 
next :: Float -> Game -> Game
next f (Tanks p e b r)
    | null e                                                             = GameOver r totalTanks --alle vijandige tanks zijn dood
    | or [co x `elem` getTankCoordinates p | x <- b]                     = GameOver r' $ totalTanks - length e --speler wordt geraakt door een kogel
    | or [co x `elem` concat [getTankCoordinates e' | e' <- e] | x <- b] = Tanks p (removeHitTanks e b) (moveBullets $ removeBullets b e) r' --vijandige tank wordt geraakt
    | otherwise                                                          = Tanks p e' (moveBullets b ++ maybeShootBullet e' r''') r'
        where r'   = getRandomNumberInRange (snd r) 0 100
              r''  = getRandomNumberInRange (snd r') 0 $ length e-1
              r''' = getRandomNumberInRange (snd r'') 0 $ length e-1
              e'   = moveTanks p e r''
next f g                                                                 = g