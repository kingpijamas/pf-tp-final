module AntColony.Logic.LoadSensing where

import open AntColony.Logic.Perceiving
import open AntColony.Model.Food
import open AntColony.Model.Terrain
import open AntColony.Utils.Maybe

senseLoad : PerceptionF LoadStatus   -- : Position -> Maybe(LoadStatus)
senseLoad pos = let getCargo occ = case occ of
                                        Ant ant -> return ant.cargo
                                        _ -> Nothing
                 in
                    (pos.occupant)            -- : Maybe(Occupant)
                     >>= getCargo             -- : Occupant -> Maybe(FoodCarrier {})
                     >>= (return . getStatus) -- : (FoodCarrier {}) -> Maybe(LoadStatus)