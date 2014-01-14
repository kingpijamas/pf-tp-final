module Model.Ant.Brain where

import open Automaton

import Ant.Seeing as See
import Ant.Smelling as Sm
import Ant.Moving as Mv
import Ant.Loading as Ld
import Ant.Scenting as Sc

type Perceiver a p = Automaton (LocationSignal) (Maybe(PerceptionSignal p))

type PerceptionSignal p = { perceived:p
                          , location:Coords
                          }

type Smeller = Perceiver Tile Pheromone
type SmellSignal = PerceptionSignal Pheromone

type Watcher = Perceiver Tile Obstacle
type SightSignal = PerceptionSignal Obstacle

data Occupant = RockTile
              | FoodTile FoodChunk
              | AntTile Ant
              | AntNestTile AntNest

scent : Pheromone

type DirectionSignal = { from:Coords
                       , targetDir:Direction
                       }

data LoadAction = Load | Unload
type LocationSignal = { from:Coords
                      , target:Coords
                      }

type LoadSignal  = { from:Coords
                   , target:Coords
                   , action:LoadAction
                   }

type ScentSignal = { target:Coords
                   , action:Action
                   }

--IN
type Watcher -- : Automaton (LocationSignal) (Maybe(SightSignal))

type Smeller -- : Automaton (LocationSignal) (Maybe(SmellSignal))

--OUT
type Motor -- : Automaton (DirectionSignal) (Maybe(Terrain))

type Loader -- : Automaton (LoadSignal) (Maybe(Terrain))

type Scenter -- : Automaton (ScentSignal) (Maybe(Terrain))


--TODO: what about perceiving multiple tiles? It would be important, especially for the smell part. 
--probably the best idea would be to have 3 sight and smell sensors at (NW, N, NE) --being N the head, that is

-- : Maybe(SightSignal) -> Maybe(SmellSignal) -> ?
behaviour terrain ant sight smell = let nothingAsZero mbx = case mbx of
                                                                Nothing -> 0
                                                                Just x -> x

                                         fullyLoaded = nothingAsZero(ant.food) == nothingAsZero(ant.limit)
                                     in
                                        case (sight.perceived, smell.perceived, fullyLoaded) of
                                           Nothing, Nothing, False -> --walk randomly
                                           Nothing, Nothing, True -> --should never happen. In any case, walk randomly
                                           Nothing, Just ph, False -> -- follow the pheromone
                                           Nothing, Just ph, True -> -- ? turn back? follow the pheromone? 
                                           Just (AntNestTile _), _ , False -> -- turn 
                                           Just (AntNestTile _), _ , True -> -- unload
                                           Just (FoodChunkT _), _ , False -> -- load
                                           Just (FoodChunkT _), _ , Turn -> -- turn 180º
                                           _ , _, _ -> -- avoid

