module AntColony.Model.Smelling where

import open AntColony.Capacities.Perceiving
import open AntColony.Model.Data.Terrain
import open AntColony.Utils.MaybeMonad

type SmellSignal = PerceptionSignal Pheromone

type SmellF = PerceptionF Tile Pheromone 

smell : SmellF  -- : Tile -> Maybe(Pheromone) 
smell tile = tile.scent

type Smeller = Perceiver Pheromone -- : SF (LocationSignal) (Maybe(Pheromone))

smeller : Terrain -> Smeller
smeller terrain = perceiver smell terrain
