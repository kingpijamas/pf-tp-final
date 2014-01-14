module Model.Rotating where

import Capacities.Rotating as Rt
import open Model.Orientation
import open Utils.MaybeMonad
import open Geography.Direction
import open Model.Terrain

type RotationF = Rt.RotationF Tile -- : Terrain -> Coords -> Maybe(Terrain)

rotate : (Direction->Direction) -> Terrain -> Coords -> Maybe(Terrain)
rotate rf terrain pos = let asRotatable tile = case tile.occupant of
                                                   AntTile ant -> return (asAnt (rotate' ant))
                                                   _ -> Nothing

                            rotate' rot = { rot | orientation <- (rf rot.orientation) }

                            updateTerrain tile' = add terrain pos tile'
                         in 
                            (terrain `get` pos)  -- : Maybe(Tile)
                             >>= (rotate)        -- : Tile -> Maybe(Tile)
                             >>= (updateTerrain) -- : Tile -> Maybe(Terrain)

clck : RotationF
clck = rotate rght

cntrclck : RotationF
cntrclck = rotate left


type Rotor = Rt.Rotor Tile -- : Automaton (DirectionSignal) (Maybe(Terrain))

rotor : Terrain -> Rotor
rotor area = Rt.rotor clck cntrclck area