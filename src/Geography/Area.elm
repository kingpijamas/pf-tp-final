module Geography.Area where

import Utils.Matrix as M


{-- Coords --}
type Coords = M.Position
coords = M.position

getX:Coords->Int
getX coords = M.row coords

getY:Coords->Int
getY coords = M.col coords

addCoord:Coords->Coords->M.Position
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
type LocationSignal a = { who:Locatable a
                        , target:Coords
                        }

locationSignal:Locatable a -> Coords -> LocationSignal a
locationSignal who target = {who=who, target=target}