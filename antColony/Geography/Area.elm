module Geography.Area where

import open Utils.Matrix

{-- Coords --}
type Coords = Position
coords = position

getX : Coords -> Int
getX coords = row coords

getY : Coords -> Int
getY coords = col coords

addCoord : Coords -> Coords -> Position
addCoord a b = let x = (getX a)+(getX b)
                   y = (getY a)+(getY b)
                in
                   coords x y 


{-- Area --}
type Area a = Matrix a
get = get
add = add
remove = remove


{-- Locatable --}
type Locatable a = { a | location:Coords }