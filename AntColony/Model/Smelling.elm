module AntColony.Model.Smelling where

import open AntColony.Capacities.Perceiving
import open AntColony.Model.Data.Terrain
import open AntColony.Model.Data.Scentable

type SmellSignal = PerceptionSignal Pheromone

smell : PerceptionF Tile Pheromone -- : Tile -> Maybe(Pheromone) 
smell tile = tile.scent

--type Smeller = Perceiver Pheromone -- : SF (LocationSignal) (Maybe(Pheromone))

--smeller : Terrain -> Smeller
--smeller terrain = perceiver smell terrain
