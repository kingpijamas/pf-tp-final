module Geography.Area where

import Utils.Matrix as M


{-- Coords --}
type Coords = M.Position
coords = M.position

getX : Coords -> Int
getX coords = M.row coords

getY : Coords -> Int
getY coords = M.col coords

addCoord : Coords -> Coords -> M.Position
addCoord a b = let x = (getX a)+(getX b)
                   y = (getY a)+(getY b)
                in
                   coords x y 


{-- Area --}
type Area a = M.Matrix a
get = M.get
add = M.add
remove = M.remove


{-- Locatable --}
type Locatable a = { a | location:Coords }

{-- Signals --}
type LocationSignal = { from:Coords
                      , target:Coords
                      }

locationSignal : Coords -> Coords -> LocationSignal
locationSignal from target = {from=from, target=target}