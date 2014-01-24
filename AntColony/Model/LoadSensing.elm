module AntColony.Model.LoadSensing where

import open AntColony.Model.Perceiving
import open AntColony.Model.Data.Food
import open AntColony.Model.Data.Terrain
import open AntColony.Utils.MaybeMonad

type Cargo = FoodCarrier {}

type WeightIntent = Perception Cargo

senseLoad : PerceptionF Cargo   -- : Position -> Maybe(Cargo)
senseLoad pos = let getCargo occ = case occ of
                                        Ant ant -> return ant.cargo
                                        _ -> Nothing
                 in
                    (pos.occupant)      -- : Maybe(Occupant)
                     >>= getCargo       -- : Occupant -> Maybe(Cargo)

--type LoadSensor = Perceiver Cargo -- : SF (LocationIntent) (Maybe(WeightIntent))

--loadSensor : Terrain -> LoadSensor
--loadSensor terrain = perceiver feelLoad terrain