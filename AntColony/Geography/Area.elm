module AntColony.Geography.Area where

import Dict

{- Coords -}
type Coords = (Int,Int)

coords:Int->Int->Coords
coords x y = (x,y)

getX:Coords->Int
getX (x,y) = x

getY:Coords->Int
getY (x,y) = y

addCoord : Coords -> Coords -> Coords
addCoord a b = let x = (getX a) + (getX b)
                   y = (getY a) + (getY b)
                in 
                   coords x y

{- Area -}
type Area v = { elems:Dict.Dict Coords v
              , width:Int
              , height:Int
              }

minX = 0
minY = 0

area':Int->Int->(Dict.Dict Coords v)->(Area v)
area' width height elems = { elems=elems
                              , width=width
                              , height=height
                              }

area:Int->Int->[(Coords,v)]->(Area v)
area width height elems = area' width height (Dict.fromList elems)

empty:Int->Int->(Area v)
empty width height = area width height []

isWithinBounds:Coords->(Area v)->Bool
isWithinBounds (x,y) area = let isBtwn x (lb,ub) = x>=lb && x<=ub
                             in
                                (x `isBtwn` (minX,area.width)) 
                                 && (y `isBtwn` (minY,area.height))

add:(Area v)->Coords->v->Maybe (Area v)
add area pos elem = if pos `isWithinBounds` area
                    then Just (area' area.width area.height (Dict.insert pos elem area.elems))
                    else Nothing

get:(Area v)->Coords->Maybe v
get area pos = Dict.lookup pos area.elems

remove:(Area v)->Coords->Maybe (Area v)
remove area pos = if pos `isWithinBounds` area
                  then Just (area' area.width area.height (Dict.remove pos area.elems))
                  else Nothing

{-- Locatable --} --TODO: check if this can be removed 
type Locatable a = { a | location:Coords }