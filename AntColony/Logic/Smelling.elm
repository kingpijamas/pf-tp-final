module AntColony.Logic.Smelling where

import open AntColony.Logic.Perceiving
import open AntColony.Model.Terrain
import open AntColony.Model.Scentable

type Smell = Pheromone

smell : PerceptionF Smell
smell pos = pos.scent