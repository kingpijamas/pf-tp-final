module AntColony.Model.Rotating where

import AntColony.Capacities.Rotating as Rt
import open AntColony.Model.Orientation
import open AntColony.Utils.MaybeMonad
import open AntColony.Geography.Direction
import open AntColony.Model.Data.Terrain

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


type Rotor = Rt.Rotor Tile -- : SF (DirectionSignal) (Maybe(Terrain))

rotor : Terrain -> Rotor
rotor area = Rt.rotor clck cntrclck area