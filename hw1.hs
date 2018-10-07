{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import CodeWorld

main :: IO ()
main = exercise3

-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

botCircle, middleCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-2.5) (solidCircle 1))
middleCircle c = colored c (translated 0   0  (solidCircle 1))
topCircle c = colored c (translated 0   2.5  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 7.5

trafficLight :: Color -> Color -> Color -> Picture
trafficLight bottom middle top  = botCircle bottom & middleCircle middle & topCircle top & frame

trafficController :: Double -> Picture
trafficController t
  | round (t) `mod` 10 < 4 = trafficLight green black black
  | round (t) `mod` 10 < 5 = trafficLight black yellow black
  | round (t) `mod` 10 < 9 = trafficLight black black red
  | otherwise              = trafficLight black yellow red

trafficLightAnimation :: Double -> Picture
trafficLightAnimation = trafficController

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2

tree :: Integer -> Picture -> Picture
tree 0 pict = pict
tree n pict = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1) pict) & rotated (- pi/10) (tree (n-1) pict))
  
blossom :: Double -> Picture
blossom d = colored green (solidCircle d)

bloom :: Double -> Picture
bloom t = tree 8 (blossom (0.05 * t))
  
bloomingTree :: Double -> Picture  
bloomingTree t 
  | t < 10 = bloom t
  | otherwise = bloom 10


exercise2 :: IO ()
exercise2 = animationOf bloomingTree

-- Exercise 3

pixel :: Picture
pixel = solidRectangle 1 1

coloredPixel :: Color -> Picture
coloredPixel c = colored c pixel

wall, ground, storage, box :: Picture
wall =    coloredPixel (gray 0.5)
ground =  coloredPixel yellow
storage = solidCircle 0.3 & ground
box =     coloredPixel brown

drawTile :: Integer -> Picture
drawTile i 
  | i == 1 = wall
  | i == 2 = ground
  | i == 3 = storage
  | i == 4 = box
  | otherwise = blank

after :: Picture -> Picture -> Picture
p1 `after` p2 = p2 & (translated 0 (-1) p1)

nextTo :: Picture -> Picture -> Picture
p1 `nextTo` p2 = p2 & (translated (-1) 0 p1)

drawTileMaze :: Integer -> Integer -> Picture
drawTileMaze x y = drawTile (maze x y)

rowOf :: Integer -> Integer -> Picture
rowOf (-11) y = blank
rowOf x y = (rowOf (x - 1) y) `nextTo` (drawTileMaze x y)

columnOf :: Integer -> Picture
columnOf (-11) = blank
columnOf y = (columnOf (y - 1)) `after` (rowOf 10 (y))

pictureOfMaze :: Picture
pictureOfMaze = columnOf 10

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze
         
maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
 
