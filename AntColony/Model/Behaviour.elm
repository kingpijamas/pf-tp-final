module AntColony.Model.Data.Ant.Brain where

import open AntColony.Utils.SignalFunction
import open List



import AntColony.Ant.Seeing as See
import AntColony.Ant.Smelling as Sm
import AntColony.Ant.Moving as Mv
import AntColony.Ant.Loading as Ld
import AntColony.Ant.Scenting as Sc

type Perceiver a p = SF (LocationIntent) (Maybe(Perception p))

type Perception p = { perceived:p
                    , location:Coords
                    }

type Smeller = Perceiver Tile Pheromone
type SmellIntent = Perception Pheromone

type Watcher = Perceiver Tile Obstacle
type SightIntent = Perception Obstacle

data Occupant = RockTile
              | FoodTile FoodChunk
              | AntTile Ant
              | AntNestTile AntNest

scent : Pheromone

type DirectionIntent = { from:Coords
                       , targetDir:Direction
                       }

data LoadAction = Load | Unload
type LocationIntent = { from:Coords
                      , target:Coords
                      }

type LoadIntent  = { from:Coords
                   , target:Coords
                   , action:LoadAction
                   }

type ScentIntent = { target:Coords
                   , action:Action
                   }

type RotationIntent = { from:Coords
                      , sense:RotationSense
                      }

--IN
type Watcher -- : SF (LocationIntent) (Maybe(SightIntent))

type Smeller -- : SF (LocationIntent) (Maybe(SmellIntent))

type LoadSensor = Perceiver Tile Cargo -- : SF (LocationIntent) (Maybe(WeightIntent))

--OUT
type Motor -- : SF (DirectionIntent) (Maybe(Terrain))

type Loader -- : SF (LoadIntent) (Maybe(Terrain))

type Scenter -- : SF (ScentIntent) (Maybe(Terrain))

type Rotor -- : SF (RotationIntent) (Maybe(Terrain))

--TODO: what about perceiving multiple tiles? It would be important, especially for the smell part. 
--probably the best idea would be to have 3 sight and smell sensors at (NW, N, NE) --being N the head, that is

--type BehaviourF

--state : b -> (a -> b -> b) -> SF a b
--hiddenState : s -> (a -> s -> (s,b)) -> SF a b

--(Terrain, Coords) -> (a -> (Terrain, Coords) -> (Terrain, Coords)) -> SF a (Terrain, Coords)

--[SF a b] -> SF a [b]

--??? : SF (Terrain,Coords) Maybe(Terrain)


--switch : SF a (b, Maybe c) -> (c->SF a b) -> SF a b

sensingBrain : (Terrain,Ant) -> SF LocationIntent ([Maybe(SightIntent)],[Maybe(SmellIntent)],Maybe(WeightIntent))
sensingBrain (terrain,ant) = let perceptor' pf dir = arr (perceiveInDir dir pf terrain)    -- : PerceptionF a p -> Direction -> Perceiver a p
                                  
                                 eye = perceptor' see                      -- : Direction -> Perceiver Tile Obstacle
                                 antenna = perceptor' smell                -- : Direction -> Perceiver Tile Pheromone
                                 loadSensor = perceptor' feelLoad          -- : Direction -> Perceiver Tile Cargo

                                 orientation = ant.orientation
                                 sensingDirs = [lft orientation, orientation, rght orientation]

                                 eyes = combine (map eye sensingDirs)            -- : SF LocationIntent [Maybe(SightIntent)]
                                 antennae = combine (map antennae sensingDirs)   -- : SF LocationIntent [Maybe(SmellIntent)]
                              in
                                (eyes &&& antennae &&& loadSensor) >>> (arr flatten)  -- : SF LocationIntent ([Maybe(SightIntent)],[Maybe(SmellIntent)],Maybe(WeightIntent))



behaviour : ([Maybe(SightIntent)],[Maybe(SmellIntent)],Maybe(WeightIntent)) -> ???
behaviour (sight,smell,load) = case (sight, smell, load) of
                                    [], [], False -> -- walk randomly
                                    [], [], True -> -- should never happen. In any case, walk randomly
                                    [], Just ph, False -> -- follow the pheromone
                                    [], Just ph, True -> -- ? turn back? follow the pheromone? 
                                    Just (AntNestTile _), _ , False -> -- turn 
                                    Just (AntNestTile _), _ , True -> -- unload
                                    Just (FoodChunkTile _), _ , False -> -- load
                                    Just (FoodChunkTile _), _ , Turn -> -- turn 180º
                                    _ , _, _ -> -- avoid











turnRight : (Terrain,Ant) -> Maybe(Terrain,Ant)
turnRight (terrain,ant) = rotor 


turn180Right : Terrain -> Coords -> 
turn180Right from  = rotor >>>


turn180Left



