module AntColony.Geography.Area where

import Dict
import open AntColony.Geography.Coords

{- Area -}
type Area v = { elems:Dict.Dict Coords v
              , width:Int
              , height:Int
              }

minX = 0
minY = 0

area' : Int -> Int -> (Dict.Dict Coords v) -> (Area v)
area' width height elems = { elems=elems
                           , width=width
                           , height=height
                           }

area : Int -> Int -> [(Coords,v)] -> (Area v)
area width height elems = area' width height (Dict.fromList elems)

empty : Int -> Int -> Area v
empty width height = area width height []

isWithinBounds : Coords -> Area v -> Bool
isWithinBounds (x,y) area = let isBtwn x (lb,ub) = x>=lb && x<=ub
                             in
                                (x `isBtwn` (minX,area.width)) 
                                 && (y `isBtwn` (minY,area.height))

add : Area v -> Coords -> v -> Maybe(Area v)
add area pos elem = if pos `isWithinBounds` area
                    then Just (area' area.width area.height (Dict.insert pos elem area.elems))
                    else Nothing

get : Area v -> Coords -> Maybe v
get area pos = Dict.lookup pos area.elems

remove : Area v -> Coords -> Maybe(Area v)
remove area pos = if pos `isWithinBounds` area
                  then Just (area' area.width area.height (Dict.remove pos area.elems))
                  else Nothing

values : Area v -> [v]
values area = Dict.values area.elems

toList : Area v -> [(Coords,v)]
toList area = Dict.toList area.elems