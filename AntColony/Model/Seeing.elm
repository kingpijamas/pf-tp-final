module AntColony.Model.Seeing where

import open AntColony.Capacities.Perceiving
import open AntColony.Model.Data.Terrain
import open AntColony.Utils.MaybeMonad

type Obstacle = Occupant

type SightSignal = PerceptionSignal Obstacle

type SightF = PerceptionF Tile Obstacle

see : SightF  -- : Tile -> Maybe(Obstacle) 
see tile = tile.occupant

type Watcher = Perceiver Obstacle -- : SF (LocationSignal) (Maybe(SightSignal))

watcher : Terrain -> Watcher
watcher terrain = perceiver see terrain