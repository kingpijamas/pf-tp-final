module AntColony.Geography.Coords where

type Coords = (Int,Int)

coords : Int -> Int -> Coords
coords x y = (x,y)

getX : Coords -> Int
getX (x,y) = x

getY : Coords -> Int
getY (x,y) = y

addCoord : Coords -> Coords -> Coords
addCoord a b = let x = (getX a) + (getX b)
                   y = (getY a) + (getY b)
                in 
                   coords x y

subtCoord : Coords -> Coords -> Coords
subtCoord a (x,y) = a `addCoord` (coords (-x) (-y))