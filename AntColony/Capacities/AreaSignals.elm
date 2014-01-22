module AntColony.Capacities.AreaSignals where
import open AntColony.Geography.Area
import open AntColony.Geography.Direction
import open AntColony.Utils.MaybeMonad

type LocationSignal = { from:Coords
                      , target:Coords
                      }

locationSignal : Coords -> Coords -> LocationSignal
locationSignal from target = { from = from
                             , target = target
                             }

type DirectionSignal = { from:Coords
                       , targetDir:Direction
                       }

directionSignal : Coords -> Direction -> DirectionSignal
directionSignal from dir = { from = from
                           , dir = dir
                           }

toLocSig : DirectionSignal -> Maybe (LocationSignal)
toLocSig dSig = let from = dSig.from

                    to = from `addDir` (dSig.targetDir)

                    locationSignal' to = return (locationSignal from to)
                 in
                    to >>= locationSignal'