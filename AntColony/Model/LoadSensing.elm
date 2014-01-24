module AntColony.Model.LoadSensing where

import open AntColony.Capacities.Perceiving
import open AntColony.Model.Data.Food
import open AntColony.Model.Data.Terrain
import open AntColony.Utils.MaybeMonad

type Cargo = FoodCarrier {}

type WeightSignal = PerceptionSignal Cargo

senseLoad : PerceptionF Tile Cargo  -- : Tile -> Maybe(Cargo)
senseLoad tile = let getCargo occ = case occ of
                                         AntTile ant -> return ant.cargo
                                         _ -> Nothing
                  in
                     (tile.occupant)     -- : Maybe(Occupant)
                      >>= getCargo       -- : Occupant -> Maybe(Cargo)

--type LoadSensor = Perceiver Cargo -- : SF (LocationSignal) (Maybe(WeightSignal))

--loadSensor : Terrain -> LoadSensor
--loadSensor terrain = perceiver feelLoad terrain