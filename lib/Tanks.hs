{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Tanks (
    tanksMain,
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
      Tank(Tank),
      tuplesSum)

tankDirections :: [Direction]
tankDirections = [north, east, south, west]

test :: Coord -> [Coord]
test t = fmap (tuplesSum t) allDirections

getTankCoordinates :: Tank -> [Coord]
getTankCoordinates t = fmap (tuplesSum $ center t) (removeWindDirections $ tdir t) ++ [center t]

-- Dit geeft alle coordinaten terug rond het centrum van een tank
-- getTankCollisionZone :: Tank -> [Coord]
-- getTankCollisionZone t = fmap (tuplesSum $ center t) allDirections ++ [center t]

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

-- Schiet een kogel in de gegeven richting
shootBullet :: Tank -> Coord
shootBullet t = tuplesSum (tankHead (center t) $ tdir t) $ tdir t

noTankCollision :: Tank -> [Tank] -> Bool
noTankCollision t ts = null $ getTankCoordinates t `intersect` concat [getTankCoordinates t' | t' <- ts]

-- moveTank :: Coord -> Direction -> [Coord] -> Coord
-- moveTank t d ts
--     | let n = tuplesSum t d
--           tc = getTankCoordinates n d
--           ts' = delete t ts, all isInBounds tc && noTankCollision n ts' = n
--     | otherwise                                                         = t

moveTank :: Tank -> Direction -> [Tank] -> Tank
moveTank t d ts
    | let n   = Tank (tuplesSum (center t) d) d
          tc  = getTankCoordinates n
          ts' = delete t ts, 
            all isInBounds tc && noTankCollision n ts' = n
    | otherwise                                        = Tank (center t) d

-- Argumenten in volgorde: coordinaat van de speler, coordinaat van de vijanden, de richtingen van de vijanden
-- Coordinaat van de speler moet meegegeven worden om collision te voorkomen tussen de vijanden en de speler
-- moveTanks :: Coord -> [Coord] -> [Direction] -> [Coord]
-- moveTanks t ts ds = [moveTank (ts !! x) (ds !! x) (t:ts) | x <- [0..length ts - 1]]
-- je geeft de speler, vijanden en een random generator mee
moveTanks :: Tank -> [Tank] -> (Int, StdGen) -> [Tank]
moveTanks t ts r = let ts' = changeDirection ts r in 
                    [moveTank x (tdir x) (t:ts')| x <- ts']

-- changeDirection :: [Direction] -> (Int, StdGen) -> [Direction]
-- changeDirection ed r = replaceN ed (fst r) $ randomDirection $ snd r
changeDirection :: [Tank] -> (Int, StdGen) -> [Tank]
changeDirection ts r = let toBeReplaced = (ts !! fst r) in 
                        Tank (center toBeReplaced) (randomDirection $ snd r) : delete toBeReplaced ts

isBulletInBounds :: Bullet -> Bool
isBulletInBounds b = isInBounds $ co b

-- moveBullets :: [Coord] -> [Direction] -> [Coord]
-- moveBullets b bd = [tuplesSum (b !! x) $ bd !! x | x <- [0..length b-1]]
moveBullets :: [Bullet] -> [Bullet]
moveBullets b = filter isBulletInBounds [TankBullet (tuplesSum (co x) $ dir x) (dir x) | x <- b]

randomDirection :: StdGen -> Direction
randomDirection stdGen =
    let r = getRandomNumberInRange stdGen 0 $ length tankDirections - 1 in
        tankDirections !! fst r

notHit :: [Bullet] -> Tank -> Bool 
notHit b t = null $ getTankCoordinates t `intersect` [co x | x <- b]

removeHitTanks :: [Tank] -> [Bullet] -> [Tank]
removeHitTanks t b = filter (notHit b) t

bulletHit :: [Tank] -> Bullet -> Bool 
bulletHit t b = null $getEnemyTankCoordinates t `intersect` [co b]   

removeBullets :: [Bullet] -> [Tank] -> [Bullet]
removeBullets b t = filter (bulletHit t) b
-- replaceN :: [a] -> Int -> a -> [a]
-- replaceN l 0 a = a:tail l
-- replaceN l n a = take(n-1) l ++ [a] ++ drop n l

-- removeN :: [a] -> Int -> [a]
-- removeN l 0 = tail l
-- removeN l n =  take(n-1) l ++ drop n l

-- argumenten in volgorde: de kogels, alle vijanden, de index van de geraakte vijand
-- destroyedTankIndex :: [Coord] -> [Coord] -> Int
-- destroyedTankIndex b ts = length $ takeWhile (==False) [ null (b `intersect` getTankCollisionZone x) | x <- ts]

startGame :: (Int, StdGen) -> Game
startGame = Tanks (Tank (0, bottom + 1) north) [Tank (-3, 6) south, Tank (3, 6) south] []
-- startGame = Tanks (Tank (0, bottom + 1) north) [Tank (-3, 6) south] []

gamePic :: Game -> Picture
gamePic (Tanks p e b r) =
    Pictures[emptyBoard, Pictures[drawCoord x | x <- getTankCoordinates p], Pictures[drawCoord x | x <- getEnemyTankCoordinates e],
    Pictures[drawCoord (co x) | x <- b]]
gamePic (GameOver r s) = displayMessage $ "Score: " ++ show s

co :: Bullet -> Coord
co (TankBullet b bd) = b

dir :: Bullet -> Coord
dir (TankBullet b bd) = bd

center :: Tank -> Coord
center (Tank t d) = t

tdir :: Tank -> Coord
tdir (Tank t d) = d

move :: Event -> Game -> Game
move (EventKey (SpecialKey KeyLeft) Down _ _) (Tanks p e b r)   = Tanks (moveTank p west e) e b r
move (EventKey (SpecialKey KeyRight) Down _ _) (Tanks p e b r)  = Tanks (moveTank p east e) e b r
move (EventKey (SpecialKey KeyDown) Down _ _) (Tanks p e b r)   = Tanks (moveTank p south e) e b r
move (EventKey (SpecialKey KeyUp) Down _ _) (Tanks p e b r)     = Tanks (moveTank p north e) e b r
move (EventKey (SpecialKey KeySpace ) Down _ _) (Tanks p e b r) = Tanks p e (b ++ [TankBullet (shootBullet p) $ tdir p]) r
move _ g                                                        = g

next :: Float -> Game -> Game
-- next f (Tanks p e ed b r)
--     | null e = GameOver r 2
--     | or [co x `elem` concat [getTankCollisionZone e' | e' <- e] | x <- b] = Tanks p d (removeN e i) (removeN ed i) b r
--     | or [co x `elem` getTankCoordinates p d | x <- b] = GameOver r $ 2 - length e
--     | otherwise = Tanks p d (moveTanks p e ed) (changeDirection ed r') (moveBullets b) r'
--                     where r' = getRandomNumberInRange (snd r) 0 $ length e
--                          i = destroyedTankIndex b e
next f (Tanks p e b r) 
    | null e                                                             = GameOver r 2 --alle vijandige tanks zijn dood
    | or [co x `elem` getTankCoordinates p | x <- b]                     = GameOver r' $ 2 - length e --speler wordt geraakt door een kogel
    | or [co x `elem` concat [getTankCoordinates e' | e' <- e] | x <- b] = Tanks p (removeHitTanks e b) (moveBullets $ removeBullets b e) r'
    | otherwise                                                          = Tanks p (moveTanks p e r'') (moveBullets b) r'
        where r'  = getRandomNumberInRange (snd r) 0 100
              r'' = getRandomNumberInRange (snd r') 0 $ length e-1
next f g = g

tanksMain :: IO ()
tanksMain  = do
        stdGen <- getStdGen
        let r = getRandomNumberInRange stdGen 0 100
        play (InWindow "UGent Tanks" (500, 800) (10, 10))
             screenGreen -- de achtergrondkleur
             2 -- aantal stappen per seconde
             (startGame r) -- de beginwereld
             gamePic -- de 'render'-functie, om naar scherm te tekenen
             move -- de 'handle'-functie, om gebruiksinvoer te verwerken
             next -- de 'step'-functie, om 1 tijdstap te laten passeren