module AntColony.Model.Remembering where

import open AntColony.Utils.MaybeMonad

import open AntColony.Model.Data.Terrain
import open AntColony.Model.Data.AntT

import open AntColony.Geography.Area



alterMemory : ([Memory] -> [Memory]) -> Terrain -> Coords -> Maybe(Terrain)
alterMemory rf terrain pos = let alterOcc pos = case pos.occupant of
                                                     Just(Ant ant) -> pos `setOccupant'` (asAnt (alterMemory' ant))
                                                     _ -> Nothing

                                 setOccupant' pos occ = return (setOccupant pos (Just occ))
  
                                 alterMemory' subj = subj `setMemory` (rf subj.remembers)
 
                                 updateTerrain pos' = add terrain pos pos'
                              in 
                                 (terrain `get` pos)   -- : Maybe(Position)
                                  >>= (alterOcc)       -- : Position -> Maybe(Position)
                                  >>= (updateTerrain)  -- : Position -> Maybe(Terrain)