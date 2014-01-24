module AntColony.Model.Data.AntT.Brain where

import open AntColony.Utils.SignalFunction
import open List



import AntColony.AntT.Seeing as See
import AntColony.AntT.Smelling as Sm
import AntColony.AntT.Moving as Mv
import AntColony.AntT.Loading as Ld
import AntColony.AntT.Scenting as Sc

type Perceiver a p = SF (LocationIntent) (Maybe(Perception p))

type Perception p = { perceived:p
                    , location:Coords
                    }

type Smeller = Perceiver Position Pheromone
type SmellIntent = Perception Pheromone

type Watcher = Perceiver Position Obstacle
type SightIntent = Perception Obstacle

data Occupant = Rock
              | Food FoodChunkT
              | Ant AntT
              | AntNest AntNestT

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

type LoadSensor = Perceiver Position Cargo -- : SF (LocationIntent) (Maybe(WeightIntent))

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

sensingBrain : (Terrain,AntT) -> SF LocationIntent ([Maybe(SightIntent)],[Maybe(SmellIntent)],Maybe(WeightIntent))
sensingBrain (terrain,ant) = let perceptor' pf dir = arr (perceiveInDir dir pf terrain)    -- : PerceptionF a p -> Direction -> Perceiver a p
                                  
                                 eye = perceptor' see                      -- : Direction -> Perceiver Position Obstacle
                                 antenna = perceptor' smell                -- : Direction -> Perceiver Position Pheromone
                                 loadSensor = perceptor' feelLoad          -- : Direction -> Perceiver Position Cargo

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
                                    Just (AntNest _), _ , False -> -- turn 
                                    Just (AntNest _), _ , True -> -- unload
                                    Just (FoodChunk _), _ , False -> -- load
                                    Just (FoodChunk _), _ , Turn -> -- turn 180º
                                    _ , _, _ -> -- avoid











turnRight : (Terrain,AntT) -> Maybe(Terrain,AntT)
turnRight (terrain,ant) = rotor 


turn180Right : Terrain -> Coords -> 
turn180Right from  = rotor >>>


turn180Left



