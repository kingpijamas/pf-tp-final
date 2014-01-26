module AntColony.Model.Data.AntT.Brain where

import open AntColony.Utils.SignalFunction
import open List

import AntColony.AntT.Seeing as See
import AntColony.AntT.Smelling as Sm
import AntColony.AntT.Moving as Mv
import AntColony.AntT.Loading as Ld
import AntColony.AntT.Scenting as Sc

type SensorData = (Maybe(Sight),Maybe(Smell),Maybe(Load))


sensors : (Terrain,AntT) -> SF Coords SensorData
sensors (terrain,ant) = let perceptor' pf dir = arr (perceiveInDir pf dir terrain)    -- : PerceptionF p -> Direction -> Perceiver p
                                  
                            eye = perceptor' see ant.orientation        -- : Direction -> Perceiver Obstacle
                            antenna = perceptor' smell ant.orientation  -- : Direction -> Perceiver Pheromone
                            loadSensor = arr (perceptor feelLoad)       -- : Direction -> Perceiver Cargo

                         in
                            (eyes &&& antennae &&& loadSensor) >>> (arr flatten)  -- : SF Coords SF



behaviour : ((Terrain,AntT), SensorData) -> (Terrain,AntT)
behaviour ((terrain,ant),(sight,smell,load)) = let getSeen = 
                                                   antP = ant.position
                                                   memory = ant.memory

                                                   move antP (antP `addDir` (antP `dirTo` ant.nestPos))

                                                   loadFrom = load terrain antP

                                                   unloadTo = unload terrain antP

                                                   turn times = clckN times terrain antP -- TODO: turn randomly!

                                                   turnL times = counterclck times terrain antP

                                                   turnR times = clckN times terrain antP

                                                   moveTowards target terrain = moveInDir terrain antP (antP `dirTo` target)

                                                   moveInDir' dir = moveInDir terrain antP dir

                                                   scent' = scent terrain


                                                case (seen, smelled, loaded) of
                                                     (Just (FoodChunk _), _, Nothing) -> loadFrom sight.target
                                                     (Just (FoodChunk _), _, Just cargo) -> turn 1
                                                     (Just (AntNest _), _, Just cargo) -> unloadTo sight.target
                                                     (Just (AntNest _), _, Nothing) -> turn 4
                                                     (Just _, _, _) -> turn 1
                                                     (_, Nothing, Just cargo) -> (scent' antP) >>= (moveTowards ant.nestPos) -- fixme: what if the spot is occupied?

                                                     -- should query the surroundings for pheromone
                                                   
                                                     (occ, Nothing, Just cargo, mem) -> case length mem of
                                                                                        1 -> turnL 1
                                                                                        2 -> turnR 2
                                                                                        3 -> case snd(mem !! 3) of
                                                                                                  lft (ant.orientation) -> case occ of
                                                                                                                                Nothing -> ...
                                                                                                                                _ -> turnR 1
                                                                                                  ant.orientation -> 
                                                                                                  rght (ant.orientation) -> case occ of
                                                                                                                                 Nothing -> ...
                                                                                                                                 _ -> turnL 1

                                                                                                  -- if the direction's different, turn there. If it isn't, get the maximum pheromone and turn that way


                                                     
                                                     (_, Just ph, Nothing) -> -- should query the surroundings for pheromone
                                                     (_, _, _) -> moveInDir (ant.orientation) -- should walk randomly!
