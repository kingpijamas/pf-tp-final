module AntColony.Model.Seeing where

import open AntColony.Model.Perceiving
import open AntColony.Model.Data.Terrain

type Sight = Occupant

see : PerceptionF Sight
see pos = pos.occupant