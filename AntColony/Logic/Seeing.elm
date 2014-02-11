module AntColony.Logic.Seeing where

import open AntColony.Logic.Perceiving
import open AntColony.Model.Terrain

type Sight = Occupant

see : PerceptionF Sight
see pos = pos.occupant