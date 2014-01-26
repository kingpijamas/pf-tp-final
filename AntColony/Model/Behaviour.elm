module AntColony.Model.Data.AntT.Brain where

import open AntColony.Utils.SignalFunction
import open List

import AntColony.AntT.Seeing as See
import AntColony.AntT.Smelling as Sm
import AntColony.AntT.Moving as Mv
import AntColony.AntT.Loading as Ld
import AntColony.AntT.Scenting as Sc

type SensorData = (Maybe(Sight),Maybe(Smell),Maybe(Load))


sensors : (Terrain,AntT) -> SF LocationIntent SensorData
sensors (terrain,ant) = let perceptor' pf dir = arr (perceiveInDir dir pf terrain)    -- : PerceptionF a p -> Direction -> Perceiver a p
                                  
                            eye = perceptor' see ant.orientation        -- : Direction -> Perceiver Position Obstacle
                            antenna = perceptor' smell ant.orientation  -- : Direction -> Perceiver Position Pheromone
                            loadSensor = arr (perceptor feelLoad)       -- : Direction -> Perceiver Position Cargo

                         in
                            (eyes &&& antennae &&& loadSensor) >>> (arr flatten)  -- : SF LocationIntent SF



behaviour : ((Terrain,AntT), SensorData) -> (Terrain,AntT)
behaviour ((terrain,ant),(sight,smell,load)) = let getSeen = 
                                                   antP = ant.position

                                                   avoid = clck -- TODO: turn randomly!
                                                   
                                                   avoid180 = clckN 4 -- TODO: turn randomly!

                                                case (seen, smelled, loaded) of
                                                     (Just (FoodChunk _), _, Nothing) -> load terrain (locationIntent sight.target antP)
                                                     (Just (FoodChunk _), _, Just cargo) -> avoid terrain antP
                                                     (Just (AntNest _), _, Just cargo) -> unload terrain (locationIntent antP sight.target)
                                                     (Just (AntNest _), _, Nothing) -> avoid180 terrain antP
                                                     (Just _, _, _) -> avoid terrain antP
                                                     (_, Nothing, Just cargo) -> avoid terrain antP
                                                     (_, Just ph, Just cargo) -> mv terrain (locationIntent antP (antP `addDir` (antP `dirTo` ant.nestPos))) -- should move and scent!
                                                     (_, _, _) -> mv terrain (locationIntent antP (antP `addDir` ant.orientation))