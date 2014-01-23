module AntColony.Capacities.Moving where

import open AntColony.Geography.Area
import open AntColony.Geography.Direction
import open AntColony.Utils.MaybeMonad
import open AntColony.Utils.SignalFunction
import open AntColony.Capacities.AreaSignals

type OccupiationF a = (Area a) -> Coords -> a -> Maybe (Area a)

type EvictionF a = (Area a) -> Coords -> Maybe ((Area a), a)

mv : (OccupiationF a) -> (EvictionF a) -> (Area a) -> LocationSignal -> Maybe (Area a)
mv occupy evict area sig = let from = sig.from
                               to = sig.target

                               occupyWith (area', mver) = occupy area' to mver
                            in
                               (area `evict` from)                  -- : Maybe (Area a, a)
                                >>= (occupyWith)                    -- : (Area a, a) -> Maybe(Area a)

type Motor a = SF (DirectionSignal) (Maybe(Area a))

motor : (OccupiationF a) -> (EvictionF a) -> (Area a) -> (Motor a)
motor occupy evict area = arr(toLocSig) >>> impure(mv occupy evict area)