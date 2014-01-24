module AntColony.Model.Smelling where

import open AntColony.Model.Perceiving
import open AntColony.Model.Data.Terrain
import open AntColony.Model.Data.Scentable

type SmellIntent = Perception Pheromone

smell : PerceptionF Pheromone -- : Position -> Maybe(Pheromone) 
smell pos = pos.scent

--type Smeller = Perceiver Pheromone -- : SF (LocationIntent) (Maybe(Pheromone))

--smeller : Terrain -> Smeller
--smeller terrain = perceiver smell terrain
