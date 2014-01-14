module Model.Seeing where

import open Capacities.Perceiving
import open Model.Terrain
import open Utils.MaybeMonad

type Obstacle = Occupant

type SightSignal = PerceptionSignal Obstacle

type SightF = PerceptionF Tile Obstacle

see : SightF  -- : Tile -> Maybe(Obstacle) 
see tile = tile.occupant

type Watcher = Perceiver Tile Obstacle -- : Automaton (LocationSignal) (Maybe(SightSignal))

watcher : Terrain -> Watcher
watcher terrain = perceiver see terrain