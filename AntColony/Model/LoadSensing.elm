module AntColony.Model.LoadSensing where

import open AntColony.Model.Perceiving
import open AntColony.Model.Data.Food
import open AntColony.Model.Data.Terrain
import open AntColony.Utils.Maybe

type Load = FoodCarrier {}

senseLoad : PerceptionF Load   -- : Position -> Maybe(Load)
senseLoad pos = let getCargo occ = case occ of
                                        Ant ant -> return ant.cargo
                                        _ -> Nothing
                 in
                    (pos.occupant)      -- : Maybe(Occupant)
                     >>= getCargo       -- : Occupant -> Maybe(Load)