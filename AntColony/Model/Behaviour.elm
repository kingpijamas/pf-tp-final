module AntColony.Model.Data.Ant.Brain where

import open AntColony.Utils.SignalFunction
import open List



import AntColony.Ant.Seeing as See
import AntColony.Ant.Smelling as Sm
import AntColony.Ant.Moving as Mv
import AntColony.Ant.Loading as Ld
import AntColony.Ant.Scenting as Sc

type Perceiver a p = SF (LocationSignal) (Maybe(PerceptionSignal p))

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

type RotationSignal = { from:Coords
                      , sense:RotationSense
                      }

--IN
type Watcher -- : SF (LocationSignal) (Maybe(SightSignal))

type Smeller -- : SF (LocationSignal) (Maybe(SmellSignal))

type LoadSensor = Perceiver Tile Cargo -- : SF (LocationSignal) (Maybe(WeightSignal))

--OUT
type Motor -- : SF (DirectionSignal) (Maybe(Terrain))

type Loader -- : SF (LoadSignal) (Maybe(Terrain))

type Scenter -- : SF (ScentSignal) (Maybe(Terrain))

type Rotor -- : SF (RotationSignal) (Maybe(Terrain))

--TODO: what about perceiving multiple tiles? It would be important, especially for the smell part. 
--probably the best idea would be to have 3 sight and smell sensors at (NW, N, NE) --being N the head, that is

--type BehaviourF

--state : b -> (a -> b -> b) -> SF a b
--hiddenState : s -> (a -> s -> (s,b)) -> SF a b

--(Terrain, Coords) -> (a -> (Terrain, Coords) -> (Terrain, Coords)) -> SF a (Terrain, Coords)

--[SF a b] -> SF a [b]

--??? : SF (Terrain,Coords) Maybe(Terrain)


--switch : SF a (b, Maybe c) -> (c->SF a b) -> SF a b

sensingBrain : (Terrain,Ant) -> SF LocationSignal ([Maybe(SightSignal)],[Maybe(SmellSignal)],Maybe(WeightSignal))
sensingBrain (terrain,ant) = let perceptor' pf dir = arr (perceiveInDir dir pf terrain)    -- : PerceptionF a p -> Direction -> Perceiver a p
                                  
                                 eye = perceptor' see                      -- : Direction -> Perceiver Tile Obstacle
                                 antenna = perceptor' smell                -- : Direction -> Perceiver Tile Pheromone
                                 loadSensor = perceptor' feelLoad          -- : Direction -> Perceiver Tile Cargo

                                 orientation = ant.orientation
                                 sensingDirs = [lft orientation, orientation, rght orientation]

                                 eyes = combine (map eye sensingDirs)            -- : SF LocationSignal [Maybe(SightSignal)]
                                 antennae = combine (map antennae sensingDirs)   -- : SF LocationSignal [Maybe(SmellSignal)]
                              in
                                (eyes &&& antennae &&& loadSensor) >>> (arr flatten)  -- : SF LocationSignal ([Maybe(SightSignal)],[Maybe(SmellSignal)],Maybe(WeightSignal))



behaviour : ([Maybe(SightSignal)],[Maybe(SmellSignal)],Maybe(WeightSignal)) -> ???
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



