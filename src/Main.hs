{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List ( (\\), intersect )
import Data.Tuple ( swap )
import Graphics.Gloss
    ( pictures,
      rectangleSolid,
      rectangleWire,
      translate,
      play,
      makeColorI,
      Display(InWindow),
      Color,
      Picture(Text, Color, Pictures, Scale) )
import Graphics.Gloss.Interface.Pure.Game
    ( pictures,
      rectangleSolid,
      rectangleWire,
      translate,
      play,
      makeColorI,
      Display(InWindow),
      Color,
      Picture(Text, Color, Pictures, Scale),
      Key(SpecialKey),
      KeyState(Down),
      SpecialKey(KeySpace, KeyLeft, KeyRight, KeyDown, KeyUp, KeyF5),
      Event(EventKey) )
import Text.ParserCombinators.ReadPrec (prec)
import System.Random (StdGen, getStdGen, randomR)
import EngineModule

-- type X = Int
-- type Y = Int
-- type Coord = (X, Y)
-- type Direction = Coord

data Game
  = Playing { player :: [Coord],
              direction :: Direction,
              apple :: Coord,
              random :: (Int, StdGen)
            }
  | GameOver {
              random :: (Int, StdGen)
            }

width :: X
width   = 10 -- de breedte van het bord
height :: Y
height  = 20 -- de hoogte van het bord
dblock :: Int
dblock  = 12 -- de zijde van 1 blokje (inclusief marge rondom)
dwidth :: Float
dwidth  = 10 -- de zijde van 1 blokje (exclusief marge, inclusief randje)
dinner :: Float
dinner  = 7 -- de zijde van 1 blokje (enkel het zwarte stuk middenin)
fscale :: Float
fscale  = 3 -- algemene schaal van de hele tekening
north :: Direction
north = (0, 1)
east :: Direction
east = (1, 0)
south :: Direction
south = (0, -1)
west :: Direction
west = (-1, 0)

bottom, top :: Y
left, right :: X
bottom = -(height `div` 2)
top    = height `div` 2
left   = -(width `div` 2)
right  = width `div` 2

filled :: Picture
filled = Scale fscale fscale $ Pictures[rectangleWire dwidth dwidth,rectangleSolid dinner dinner]

-- Een lege pixel, gecentreerd rond de oorsprong
empty :: Picture
empty = Color screenGray filled

-- Maakt een rij van lege pixels
makeRow :: Y -> Picture
makeRow y = pictures[drawEmpty (x, y) | x <- [left..right]]

-- Een bord met enkel lege pixels, gecentreerd rond de oorsprong
emptyBoard :: Picture
emptyBoard = Pictures[makeRow y | y <- [bottom..top]]

-- Past het coordinaat van een pixel aan om het juist te kunnen translaten
adjustSize :: Int -> Float
adjustSize v = fromIntegral (round fscale * dblock * v)

-- Een gevulde/actieve pixel op de locatie aangeduid door de coördinaat.
drawCoord :: Coord -> Picture
drawCoord c = drawPicture c filled

-- Een lege pixel op de locatie aangeduid door de coördinaat.
drawEmpty :: Coord -> Picture
drawEmpty c = drawPicture c empty

-- Hulpfunctie die gebruikt wordt om zowel tegels als gevulde pixels mee te tekenen
drawPicture :: Coord -> Picture -> Picture
drawPicture c = translate (adjustSize $ fst c) (adjustSize $ snd c)

gamePic :: Game -> Picture
gamePic (Playing p d a r) = Pictures[emptyBoard, Pictures[drawCoord x | x <- p], drawCoord a]
gamePic (GameOver r) = displayMessage "Game Over!"

displayMessage :: String -> Picture
displayMessage m = Scale 0.5 0.5 $ translate (adjustSize (-width)) 0 $ Text m

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


-- Geeft de som van de elementen van 2 tuples terug
tuplesSum :: Coord -> Direction -> Coord
tuplesSum (x, y) (x', y') = (x + x', y + y')

-- Kijkt of een coordinaat op het veld ligt
isInBounds :: Coord -> Bool
isInBounds (x, y) = x >= left && x <= right && y >= bottom && y <= top

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

lengthenSnake :: Direction -> [Coord] -> [Coord]
lengthenSnake d p
  | tuplesSum l south `elem` p = p ++ [tuplesSum l north]
  | tuplesSum l north `elem` p = p ++ [tuplesSum l south]
  | tuplesSum l west `elem` p  = p ++ [tuplesSum l east]
  | tuplesSum l east `elem` p  = p ++ [tuplesSum l west]
  | length p == 1              = p ++ [tuplesSum l $ getOppositeDirection d]
    where l = last p

next :: Float -> Game -> Game
next f (Playing p d a r)
  | let newHead = tuplesSum (head p) d in not (isInBounds newHead) || newHead `elem` p = GameOver r -- slang raakt de muur of zichzelf
  | a == head p                                                                        = getNewAppleLocation (Playing (lengthenSnake d p) d a r)
  | otherwise                                                                          = Playing (tuplesSum (head p) d : init p) d a r
next t (GameOver r)                                                                    = GameOver r

-- F5 wordt gebruikt om het spel opnieuw te starten
move :: Event -> Game -> Game
move (EventKey (SpecialKey KeyLeft) Down _ _) (Playing p d a r)  = Playing p (preventOppositeDirection west d) a r
move (EventKey (SpecialKey KeyRight) Down _ _) (Playing p d a r) = Playing p (preventOppositeDirection east d) a r
move (EventKey (SpecialKey KeyDown) Down _ _) (Playing p d a r)  = Playing p (preventOppositeDirection south d) a r
move (EventKey (SpecialKey KeyUp) Down _ _) (Playing p d a r)    = Playing p (preventOppositeDirection north d) a r
move (EventKey (SpecialKey KeyF5 ) Down _ _) (GameOver r)        = startGame $ getRandomNumberInRange (snd r) 0 $ height*width-1
move _ g                                                         = g

startGame :: (Int, StdGen) -> Game
startGame r = Playing [(0, 0)] north (getBoardCoordinates !! fst r ) r

main :: IO ()
main  = do
        stdGen <- getStdGen
        let r = getRandomNumberInRange stdGen 0 $ height*width-1
        play (InWindow "UGent Snake" (500, 800) (10, 10))
             screenGreen -- de achtergrondkleur
             2 -- aantal stappen per seconde
             (startGame r) -- de beginwereld
             gamePic -- de 'render'-functie, om naar scherm te tekenen
             move -- de 'handle'-functie, om gebruiksinvoer te verwerken
             next -- de 'step'-functie, om 1 tijdstap te laten passeren
