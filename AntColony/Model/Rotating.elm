module AntColony.Model.Rotating where

import open AntColony.Utils.MaybeMonad

import open AntColony.Model.Data.Terrain



import open AntColony.Geography.Area
import open AntColony.Geography.Direction

rotate : (Direction -> Direction) -> Terrain -> Coords -> Maybe(Terrain)
rotate rf terrain pos = let rotateOcc pos = case pos.occupant of
                                                 Just(Ant ant) -> setOccupant' pos (asAnt (rotate' ant))
                                                 _ -> Nothing

                            setOccupant' pos occ = return (setOccupant pos (Just occ))

                            rotate' rot = { rot | orientation <- (rf rot.orientation) }

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