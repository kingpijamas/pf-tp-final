module AntColony.Capacities.Positioning where
  
import open AntColony.Geography.Area
import open AntColony.Geography.Direction
import open AntColony.Utils.MaybeMonad

type LocationIntent = { from:Coords
                      , target:Coords
                      }

locationIntent : Coords -> Coords -> LocationIntent
locationIntent from target = { from = from
                             , target = target
                             }

type DirectionIntent = { from:Coords
                       , targetDir:Direction
                       }

directionIntent : Coords -> Direction -> DirectionIntent
directionIntent from dir = { from = from
                           , targetDir = dir
                           }

toLocSig : DirectionIntent -> Maybe (LocationIntent)
toLocSig dSig = let from = dSig.from

                    to = from `addDir` (dSig.targetDir)

                    locationIntent' to = return (locationIntent from to)
                 in
                    to >>= locationIntent'