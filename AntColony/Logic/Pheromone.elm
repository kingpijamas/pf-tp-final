module AntColony.Logic.Pheromone where

import open AntColony.Utils.Maybe
import open AntColony.Utils.SignalFunction
import open AntColony.Geography.Area
import open AntColony.Model.Terrain
import open AntColony.Logic.Scenting

updateAll : SF Terrain (Maybe(Terrain))
updateAll = let getPositions terrain = map (\(coords,pos)->coords) (filter (\(coords,pos) -> isJust pos.scent) (toList terrain))
             in
                ((arr return) &&& (arr getPositions))                                            -- : SF Terrain (Maybe(Terrain), [Coords])
                 >>> (loopUntil update (\(mbterr,coords) -> isNothing mbterr || isEmpty coords)) -- : SF (Maybe(Terrain), [Coords]) (Maybe(Terrain))

update : SF (Maybe(Terrain), [Coords]) (Maybe(Terrain), [Coords])
update = let getFirst = arr (\(justTerrain, poss) -> case justTerrain of
                                                          Just terrain -> ((terrain, head poss), tail poss))
          in 
             getFirst                                -- : SF (Maybe(Terrain), [Coords]) ((Terrain, Coords), [Coords])
              >>> (first (arr <| uncurry unscent))   -- : SF ((Terrain, Coords), [Coords]) (Maybe[Terrain], [Coords])