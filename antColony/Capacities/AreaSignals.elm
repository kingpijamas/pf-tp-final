module Capacities.AreaSignals where
import open Geography.Area
import open Geography.Direction
import open Utils.MaybeMonad

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

toLocSig : DirectionSignal -> Maybe (LocationSignal)
toLocSig dSig = let from = dSig.from

                    to = from `addDir` (dSig.targetDir)

                    locationSignal' to = return (locationSignal from to)
                 in
                    to >>= locationSignal'