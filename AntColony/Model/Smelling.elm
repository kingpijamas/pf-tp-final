module AntColony.Model.Smelling where

import open AntColony.Model.Perceiving
import open AntColony.Model.Data.Terrain
import open AntColony.Model.Data.Scentable

type Smell = Perception Pheromone

smell : PerceptionF Pheromone
smell pos = pos.scent