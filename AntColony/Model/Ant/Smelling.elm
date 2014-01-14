module AntColony.Model.Ant.Smelling where

import open AntColony.Capacities.Perceiving
import open AntColony.Model.Terrain
import open AntColony.Utils.MaybeMonad

type SmellSignal = PerceptionSignal Pheromone

type SmellF = PerceptionF Tile Pheromone 

smell : SmellF  -- : Tile -> Maybe(Pheromone) 
smell tile = if (tile.scent == 0)
             then Nothing
             else return (tile.scent)

type Smeller = Perceiver Tile Pheromone

smeller : Terrain -> Smeller
smeller terrain = perceiver smell terrain
