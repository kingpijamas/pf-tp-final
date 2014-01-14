module AntColony.Capacities.Rotating where

import open AntColony.Geography.Direction
import open AntColony.Capacities.AreaSignals
import open AntColony.Utils.MaybeMonad
import open Automaton

data RotationSense = Clockwise | Counterclockwise

type RotationSignal = { from:Coords
                      , sense:RotationSense
                      }

rotationSignal : Coords -> RotationSense -> RotationSignal
rotationSignal from sense = { from=from
                            , sense=sense
                            }

type RotationF a = (Area a) -> Coords -> Maybe(Area a)

rotationProxy : (RotationF a) -> (RotationF a) -> (Area a) -> RotationSignal -> Maybe(Area a)
rotationProxy clck cntrclck area sig = let rf = case sig.sense of
                                                    Clockwise -> clck
                                                    Counterclockwise -> cntrclck
                                        in
                                           rf area (sig.from)


type Rotor a =  Automaton (DirectionSignal) (Maybe(Area a))

rotor : (RotationF a) -> (RotationF a) -> (Area a) -> (Rotor a)
rotor clck cntrclck area = pure (rotationProxy clck cntrclck area)