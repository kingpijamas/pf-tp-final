module AntColony.Model.Ant.Seeing where

import open AntColony.Capacities.Perceiving
import open AntColony.Model.Terrain
import open AntColony.Utils.MaybeMonad

type Obstacle = Occupant

type SightSignal = PerceptionSignal Obstacle

type SightF = PerceptionF Tile Obstacle

see : SightF  -- : Tile -> Maybe(Obstacle) 
see tile = tile.occupant

type Watcher = Perceiver Tile Obstacle

watcher : Terrain -> Watcher
watcher terrain = perceiver see terrain