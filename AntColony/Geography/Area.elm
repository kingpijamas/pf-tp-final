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

terrain':Int->Int->(Dict.Dict Coords v)->(Area v)
terrain' width height elems = { elems=elems
                              , width=width
                              , height=height
                              }

terrain:Int->Int->[(Coords,v)]->(Area v)
terrain width height elems = terrain' width height (Dict.fromList elems)

empty:Int->Int->(Area v)
empty width height = terrain width height []

isWithinBounds:Coords->(Area v)->Bool
isWithinBounds (x,y) terrain = let isBtwn x (lb,ub) = x>=lb && x<=ub
                             in
                                (x `isBtwn` (minX,terrain.width)) 
                                 && (y `isBtwn` (minY,terrain.height))

add:(Area v)->Coords->v->Maybe (Area v)
add terrain pos elem = if pos `isWithinBounds` terrain
                    then Just (terrain' terrain.width terrain.height (Dict.insert pos elem terrain.elems))
                    else Nothing

get:(Area v)->Coords->Maybe v
get terrain pos = Dict.lookup pos terrain.elems

remove:(Area v)->Coords->Maybe (Area v)
remove terrain pos = if pos `isWithinBounds` terrain
                  then Just (terrain' terrain.width terrain.height (Dict.remove pos terrain.elems))
                  else Nothing