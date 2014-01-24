module AntColony.Model.Smelling where

import open AntColony.Model.Perceiving
import open AntColony.Model.Data.Terrain
import open AntColony.Model.Data.Scentable

type SmellIntent = Perception Pheromone

smell : PerceptionF Pheromone -- : Tile -> Maybe(Pheromone) 
smell tile = tile.scent

--type Smeller = Perceiver Pheromone -- : SF (LocationIntent) (Maybe(Pheromone))

--smeller : Terrain -> Smeller
--smeller terrain = perceiver smell terrain
