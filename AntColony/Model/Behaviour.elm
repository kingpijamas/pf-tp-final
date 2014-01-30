module AntColony.Model.Behaviour where

import open Maybe
import open List
import open AntColony.Utils.List
import open AntColony.Utils.Tuple
import open AntColony.Utils.Maybe
import open AntColony.Utils.SignalFunction

import open AntColony.Geography.Area
import open AntColony.Geography.Direction

import open AntColony.Model.Data.Terrain
import open AntColony.Model.Data.Scentable
import open AntColony.Model.Data.AntT

import open AntColony.Model.Perceiving
import open AntColony.Model.LoadSensing
import open AntColony.Model.Loading
import open AntColony.Model.Moving
import open AntColony.Model.Rotating
import open AntColony.Model.Scenting
import open AntColony.Model.Seeing
import open AntColony.Model.Smelling

type SensorData = ([Maybe(Sight)],[Maybe(Smell)],Maybe(Load))

getSensingDirs : Direction -> [Direction]
getSensingDirs orientation = [orientation, lft orientation, rght orientation]

sensors : (Terrain,AntT) -> SF Coords SensorData
sensors (terrain,ant) = let orientation = ant.orientation
                            sensingDirs = getSensingDirs orientation

                            eyes = perceptors see sensingDirs terrain             -- : SF Coords [Maybe(Occupant)]
                            antennae = perceptors smell sensingDirs terrain       -- : SF Coords [Maybe(Pheromone)]
                            loadSensor = perceptor senseLoad terrain              -- : SF Coords Cargo
                         in
                            (eyes &&& antennae &&& loadSensor) >>> (arr flatten)  -- : SF Coords SensorData

behaviour : ((Terrain,AntT), SensorData) -> Terrain
behaviour ((terrain,ant),(seen,smelled,currLoad)) = let currPos : Coords
                                                        currPos = ant.position
                                                        
                                                        frontPos : Maybe(Coords)
                                                        frontPos = currPos `addDir` forward 

                                                        frontSight : Maybe(Sight)
                                                        frontSight = head seen

                                                        frontSmell : Maybe(Smell)
                                                        frontSmell = head smelled

                                                        forward : Direction
                                                        forward = ant.orientation

                                                        toNest : Direction
                                                        toNest = currPos `dirTo` ant.nestPos
                                                         
                                                        loadFrom : Coords -> Maybe(Terrain)
                                                        loadFrom = load terrain currPos
                                                         
                                                        unloadTo : Coords -> Maybe(Terrain)
                                                        unloadTo = unload terrain currPos

                                                        turn : Int -> Maybe(Terrain)
                                                        turn times = clckN times terrain currPos -- TODO: turn randomly!

                                                        turnAround : Maybe(Terrain)
                                                        turnAround = turn 4

                                                        scent' : Coords -> Maybe(Terrain)
                                                        scent' = scent terrain

                                                        towardsDo : Direction -> (Direction-> Maybe(Terrain)) -> Maybe(Terrain)
                                                        towardsDo goal dirF = case (findPath goal (asPaths forward seen smelled)) of
                                                                                    Just dir -> dirF dir
                                                                                    Nothing -> turn 2

                                                     in
                                                        case (frontSight, frontSmell, currLoad) of
                                                             (Just (FoodChunk _), _, Nothing)    -> frontPos >>= loadFrom 
                                                             (Just (FoodChunk _), _, Just cargo) -> turnAround -- could probably be removed
                                                             (Just (AntNest _), _, Just cargo)   -> frontPos >>= unloadTo
                                                             (Just (AntNest _), _, Nothing)      -> turnAround -- could probably be removed
                                                             (Just _, _, _)                      -> turn 1
                                                              
                                                             (_, Nothing, Just cargo) -> towardsDo toNest (\dir -> (scent' currPos) >>= (moveInDir dir))

                                                             (Nothing, Just ph, Just cargo) -> towardsDo forward (\dir -> (scent' currPos) >>= (moveInDir dir))
                                                               
                                                             (_, Just ph, Nothing) -> towardsDo forward (\dir -> moveInDir dir)

                                                             (_, _, _) -> moveInDir forward -- should walk randomly!


--filterPath : Direction -> Direction -> [Maybe(Sight)] -> [Maybe(Smell)] -> String-- Maybe(Direction)
--filterPath curr wish sight smell = let paths = 

type Path = (Direction, Maybe(Sight), Maybe(Smell))

asPaths : Direction -> [Maybe(Sight)] -> [Maybe(Smell)] -> [Path]
asPaths forward sight smell = flatZip (getSensingDirs forward) (zip sight smell)

findPath : Direction -> [Path] -> Maybe(Direction)
findPath goal paths = let isPathToGoal (dir,_,_) = goal == dir

                          occAndSmell (_,mbocc,mbsmell) = (isJust mbocc, mbsmell)

                          better p p' = case (occAndSmell p, occAndSmell p') of
                                             ((True, _), (False, _)) -> p'
                                             ((False, Just sm), (False, Just sm')) -> if sm >= sm'
                                                                                      then p
                                                                                      else p'
                                             (_,_) -> p

                          findPathIn goalP paths = foldl better goalP paths
                       in
                          case (filter isPathToGoal paths) of
                                [] -> Nothing
                                [goalP] -> case (findPathIn goalP paths) of
                                                 (dir, Nothing, _) -> Just dir
                                                 _ -> Nothing