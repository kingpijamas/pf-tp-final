module AntColony.Geography.Area where

import Dict
import Maybe

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

area:(Dict.Dict Coords v)->Int->Int->Area v
area elems width height = { elems=elems
                          , width=width
                          , height=height
                          }

area':Int->Int->Area v
area' width height = area (Dict.empty) width height

isWithinBounds:Coords->(Area v)->Bool
isWithinBounds (x,y) {elems,width,height} = let isBtwn x (lb,ub) = x>=lb && x<=ub
                                             in
                                                (x `isBtwn` (minX,width)) && (y `isBtwn` (minY,height))

add:(Area v)->Coords->v->Maybe (Area v)
add {elems,width,height} pos elem = let mat = area elems width height
                                 in
                                   if pos `isWithinBounds` mat
                                    then Just (area (Dict.insert pos elem elems) width height)
                                    else Nothing

get:(Area v)->Coords->Maybe v
get {elems,width,height} pos = Dict.lookup pos elems

remove:(Area v)->Coords->Maybe (Area v)
remove {elems,width,height} pos = let mat = area elems width height
                               in
                                 if pos `isWithinBounds` mat
                                  then Just (area (Dict.remove pos elems) width height)
                                  else Nothing


{-- Locatable --} --TODO: check if this can be removed 
type Locatable a = { a | location:Coords }