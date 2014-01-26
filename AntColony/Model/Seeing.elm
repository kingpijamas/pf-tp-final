module AntColony.Model.Seeing where

import open AntColony.Model.Perceiving
import open AntColony.Model.Data.Terrain

type Sight = Perception Occupant

see : PerceptionF Occupant
see pos = pos.occupant