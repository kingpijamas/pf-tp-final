module AntColony.Logic.Pheromone where

import open AntColony.Utils.Maybe
import open AntColony.Utils.SignalFunction
import open AntColony.Geography.Coords
import open AntColony.Model.Terrain
import open AntColony.Logic.Scenting

decayAll : SF (Maybe(Terrain)) (Maybe(Terrain))
decayAll = let isScented (coords,pos) = isJust pos.scent

               scentedCoords terrain = map fst (filter isScented (maybe [] toList terrain))

               nothingOrEmpty (mbterr,coords) = isNothing mbterr || isEmpty coords
            in
               (identity &&& (arr scentedCoords))       -- : SF (Maybe(Terrain)) (Maybe(Terrain), [Coords])
                >>> (loopUntil decay nothingOrEmpty)    -- : SF (Maybe(Terrain), [Coords]) (Maybe(Terrain))

decay : SF (Maybe(Terrain), [Coords]) (Maybe(Terrain), [Coords])
decay = let getFirst = arr (\(justTerrain, poss) -> case justTerrain of
                                                         Just terrain -> ((terrain, head poss), tail poss))
         in 
            getFirst                                -- : SF (Maybe(Terrain), [Coords]) ((Terrain, Coords), [Coords])
             >>> (first (arr <| uncurry unscent))   -- : SF ((Terrain, Coords), [Coords]) (Maybe[Terrain], [Coords])