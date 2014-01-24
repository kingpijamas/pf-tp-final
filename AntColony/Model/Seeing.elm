module AntColony.Model.Seeing where

import open AntColony.Capacities.Perceiving
import open AntColony.Model.Data.Terrain

--type Obstacle = Occupant

type SightSignal = PerceptionSignal Occupant

see : PerceptionF Tile Occupant
see tile = tile.occupant

--type Watcher = Perceiver Obstacle -- : SF (LocationSignal) (Maybe(SightSignal))

--watcher : Terrain -> Watcher
--watcher terrain = perceiver see terrain