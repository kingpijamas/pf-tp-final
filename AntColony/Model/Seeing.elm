module AntColony.Model.Seeing where

import open AntColony.Model.Perceiving
import open AntColony.Model.Data.Terrain

--type Obstacle = Occupant

type SightIntent = Perception Occupant

see : PerceptionF Occupant
see tile = tile.occupant

--type Watcher = Perceiver Obstacle -- : SF (LocationIntent) (Maybe(SightIntent))

--watcher : Terrain -> Watcher
--watcher terrain = perceiver see terrain