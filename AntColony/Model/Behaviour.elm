module AntColony.Model.Data.AntT.Brain where

import open AntColony.Utils.SignalFunction
import open List

import AntColony.AntT.Seeing as See
import AntColony.AntT.Smelling as Sm
import AntColony.AntT.Moving as Mv
import AntColony.AntT.Loading as Ld
import AntColony.AntT.Scenting as Sc

type SensorData = (Maybe(SightIntent),Maybe(SmellIntent),Maybe(WeightIntent))

sensors : (Terrain,AntT) -> SF LocationIntent SensorData
sensors (terrain,ant) = let perceptor' pf dir = arr (perceiveInDir dir pf terrain)    -- : PerceptionF a p -> Direction -> Perceiver a p
                                  
                            eye = perceptor' see ant.orientation        -- : Direction -> Perceiver Position Obstacle
                            antenna = perceptor' smell ant.orientation  -- : Direction -> Perceiver Position Pheromone
                            loadSensor = arr (perceptor feelLoad)       -- : Direction -> Perceiver Position Cargo

                         in
                            (eyes &&& antennae &&& loadSensor) >>> (arr flatten)  -- : SF LocationIntent SF



behaviour : ((Terrain,AntT), SensorData) -> (Terrain,AntT)
behaviour ((terrain,ant),sensorData) = case (sight, smell, load) of
                                            [], [], False -> -- walk randomly
                                            [], [], True -> -- should never happen. In any case, walk randomly
                                            [], Just ph, False -> -- follow the pheromone
                                            [], Just ph, True -> -- ? turn back? follow the pheromone? 
                                            Just (AntNest _), _ , False -> -- turn 
                                            Just (AntNest _), _ , True -> -- unload
                                            Just (FoodChunk _), _ , False -> -- load
                                            Just (FoodChunk _), _ , Turn -> -- turn 180º
                                            _ , _, _ -> -- avoid