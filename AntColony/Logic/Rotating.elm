module AntColony.Logic.Rotating where

import open AntColony.Utils.Maybe

import open AntColony.Model.Terrain



import open AntColony.Geography.Area
import open AntColony.Geography.Direction

rotate : (Direction -> Direction) -> Terrain -> Coords -> Maybe(Terrain)
rotate rotf terrain pos = let rotateOcc pos = case pos.occupant of
                                                   Just(Ant ant) -> return (pos `setOccupant2` (Ant <| rotate' ant))
                                                   _ -> Nothing

                              rotate' rotatable = { rotatable | orientation <- (rotf rotatable.orientation) }

                              updateTerrain pos' = add terrain pos pos'
                           in 
                              (terrain `get` pos)   -- : Maybe(Position)
                               >>= (rotateOcc)      -- : Position -> Maybe(Position)
                               >>= (updateTerrain)  -- : Position -> Maybe(Terrain)

clck : Terrain -> Coords -> Maybe(Terrain)
clck = rotate rght

clckN : Int -> Terrain -> Coords -> Maybe(Terrain)
clckN times = rotate ((flip rghtN) times)

cntrclck : Terrain -> Coords -> Maybe(Terrain)
cntrclck = rotate lft

cntrclckN : Int -> Terrain -> Coords -> Maybe(Terrain)
cntrclckN times = rotate ((flip lftN) times)
