module AntColony.Model.Rotating where

import AntColony.Capacities.Rotating as Rt
import open AntColony.Utils.MaybeMonad
import open AntColony.Model.Data.Terrain
import open AntColony.Geography.Area
import open AntColony.Geography.Direction

type RotationF = Rt.RotationF Tile -- : Terrain -> Coords -> Maybe(Terrain)

rotate : (Direction->Direction) -> Terrain -> Coords -> Maybe(Terrain)
rotate rf terrain pos = let rotateOcc tile = case tile.occupant of
                                                   Just(AntTile ant) -> setOccupant' tile (asAnt (rotate' ant))
                                                   _ -> Nothing

                            setOccupant' tile occ = return (setOccupant tile (Just occ))

                            rotate' rot = { rot | orientation <- (rf rot.orientation) }

                            updateTerrain tile' = add terrain pos tile'
                         in 
                            (terrain `get` pos)   -- : Maybe(Tile)
                             >>= (rotateOcc)      -- : Tile -> Maybe(Tile)
                             >>= (updateTerrain)  -- : Tile -> Maybe(Terrain)

clck : RotationF
clck = rotate rght

cntrclck : RotationF
cntrclck = rotate lft


type Rotor = Rt.Rotor Tile -- : SF (DirectionSignal) (Maybe(Terrain))

rotor : Terrain -> Rotor
rotor area = Rt.rotor clck cntrclck area