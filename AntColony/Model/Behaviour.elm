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
sensingBrain (terrain,ant) = let perceptor' pf dir = pure (perceiveInDir dir pf terrain)    -- : PerceptionF a p -> Direction -> Perceiver a p
                                  
                                 eye = (perceptor' see)                    -- : Direction -> Perceiver Tile Obstacle
                                 antenna = (perceptor' smell)              -- : Direction -> Perceiver Tile Pheromone
                                 loadSensor = (perceptor' feelLoad)        -- : Direction -> Perceiver Tile Cargo

                                 --type Perceiver a p = SF (LocationSignal) (Maybe(PerceptionSignal p))

                                 orientation = ant.orientation

                                 eyes = combine (map eye [lft orientation, orientation, rght orientation])       -- : [SF LocationSignal Maybe(SightSignal)] -> SF LocationSignal [Maybe(SightSignal)]
                                 antennae = combine (map eye [lft orientation, orientation, rght orientation])   -- : [SF LocationSignal Maybe(SmellSignal)] -> SF LocationSignal [Maybe(SmellSignal)]

                                 --switch3' : b -> (a -> b -> b) -> c -> (a -> c -> c) -> d -> (a -> d -> d) -> SF a (b,c,d)
                                 --switch3' : [Maybe(SightSignal)] -> (LocationSignal -> [Maybe(SightSignal)] -> [Maybe(SightSignal)]) -> ...
                              in
                                 switch3' [] eyes [] antennae Nothing loadSensor -- : SF LocationSignal ([Maybe(SightSignal)],[Maybe(SmellSignal)],Maybe(WeightSignal))



behaviour : SF ([Maybe(SightSignal)],[Maybe(SmellSignal)],Maybe(WeightSignal)) 
behaviour (eyes,antennae,load) = case (sight.perceived, smell.perceived, fullyLoaded) of
                                    Nothing, Nothing, False -> -- walk randomly
                                    Nothing, Nothing, True -> -- should never happen. In any case, walk randomly
                                    Nothing, Just ph, False -> -- follow the pheromone
                                    Nothing, Just ph, True -> -- ? turn back? follow the pheromone? 
                                    Just (AntNestTile _), _ , False -> -- turn 
                                    Just (AntNestTile _), _ , True -> -- unload
                                    Just (FoodChunkTile _), _ , False -> -- load
                                    Just (FoodChunkTile _), _ , Turn -> -- turn 180º
                                    _ , _, _ -> -- avoid











turnRight : (Terrain,Ant) -> Coords -> Terrain
turnRight = 


turn180Right : Terrain -> Coords -> 
turn180Right from  = rotor >>>


turn180Left



