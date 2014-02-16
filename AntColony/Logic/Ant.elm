module AntColony.Logic.Ant where

import open Maybe
import open List
import open AntColony.Utils.List
import open AntColony.Utils.Tuple
import open AntColony.Utils.Maybe
import open AntColony.Utils.SignalFunction

import open AntColony.Geography.Area
import open AntColony.Geography.Direction

import open AntColony.Model.Terrain
import open AntColony.Model.Scent
import open AntColony.Model.AntT

import open AntColony.Logic.Perceiving
import open AntColony.Logic.LoadSensing
import open AntColony.Logic.Loading
import open AntColony.Logic.Moving
import open AntColony.Logic.Rotating
import open AntColony.Logic.Scenting
import open AntColony.Logic.Seeing
import open AntColony.Logic.Smelling

animateAnts : SF (Maybe(Terrain)) (Maybe(Terrain))
animateAnts = let getAntss terrain = map (\occ -> case occ of
                                                       (Ant ant) -> ant) (getAnts terrain)

                  hasAnts (_,ants) = not <| isEmpty ants
               in
                  (identity &&& (impure (arr getAntss) []))                                       -- : SF (Maybe(Terrain)) (Maybe(Terrain),[AntT])
                   >>> (loopUntil animate (\(mbterr,ants) -> isNothing mbterr || isEmpty ants))   -- : SF (Maybe(Terrain), [AntT]) (Maybe(Terrain))


animate : SF (Maybe(Terrain), [AntT]) (Maybe(Terrain), [AntT])
animate = let getFirst = arr (\(justTerrain, ants) -> case justTerrain of
                                                           Just terrain -> ((terrain, head ants), tail ants))
           in 
              getFirst                            -- : SF (Maybe(Terrain), [AntT]) ((Terrain, AntT), [AntT])
               >>> (first (identity &&& sense))   -- : SF ((Terrain, AntT), [AntT]) (((Terrain, AntT),(SensorData)), [AntT])
               >>> (first (arr act))              -- : SF (((Terrain, AntT), SensorData), [AntT]) (Maybe(Terrain), [AntT])

type SensorData = ([Maybe(Sight)], [Maybe(Smell)], Maybe(Load))

getSensingDirs : Direction -> [Direction]
getSensingDirs orientation = [orientation, lft orientation, rght orientation]

sense : SF (Terrain, AntT) SensorData
sense = let dirPerceptors pf (terrain, ant) = perceiveInDirs pf (getSensingDirs ant.orientation) terrain ant.position
            eyes = arr (dirPerceptors see)                    -- : SF (Terrain, AntT) [Maybe(Sight)]
            antennae = arr (dirPerceptors smell)              -- : SF (Terrain, AntT) [Maybe(Smell)]

            perceptor pf (terrain, ant) = perceive pf terrain ant.position
            loadSensor  =  arr (perceptor senseLoad)         -- : SF (Terrain, AntT) Maybe(Cargo)
         in
            (eyes &&& antennae &&& loadSensor) >>^ (flatten)  -- : SF (Terrain, AntT) (SensorData)          

--act : ((Terrain, AntT), SensorData) -> Maybe(Terrain)
--act ((terrain,ant)
--    ,(seen,smelled,currLoad)) = return terrain

act : ((Terrain, AntT), SensorData) -> Maybe(Terrain)
act ((terrain,ant)
    ,(seen,smelled,currLoad)) = let currPos = ant.position
                                    forward = ant.orientation
                                    --frontPos = currPos `addDir` forward
                                    --toNest = currPos `dirTo` ant.nestPos

                                    --loadFrom = load terrain currPos
                                    --unloadTo = unload terrain currPos
                                    turn times = clckN times terrain currPos -- TODO: turn randomly!
                                    --turnAround = turn 4

                                    --towardsDo goal dirF = case (findPath goal (asPaths forward seen smelled)) of
                                    --                           Just dir -> dirF dir
                                    --                           Nothing -> turn 2
                                 in
                                    case (head seen, head smelled, currLoad) of
                                         --(Just (FoodChunk _), _, Nothing)    -> frontPos >>= loadFrom 
                                         --(Just (FoodChunk _), _, Just cargo) -> turnAround -- could probably be removed
                                         --(Just (AntNest _), _, Just cargo)   -> frontPos >>= unloadTo
                                         --(Just (AntNest _), _, Nothing)      -> turnAround -- could probably be removed
                                         --(Just _, _, _)                      -> turn 1
                                         --(_, Nothing, Just cargo) -> towardsDo toNest (\dir -> (scent terrain currPos)
                                         --                                                              >> (moveInDir terrain currPos dir))
                                         --(_, Just ph, Just cargo) -> towardsDo forward (\dir -> (scent terrain currPos)
                                         --                                                              >> (moveInDir terrain currPos dir))
                                         --(_, Just ph, Nothing) -> towardsDo forward (\dir -> moveInDir terrain currPos dir)
                                         --(_, _, _) -> moveInDir terrain currPos forward -- should walk randomly!
                                         (_,_,_) -> turn 1

type Path = (Direction, Maybe(Sight), Maybe(Smell))

asPaths : Direction -> [Maybe(Sight)] -> [Maybe(Smell)] -> [Path]
asPaths forward sight smell = flatZip (getSensingDirs forward) (zip sight smell)

findPath : Direction -> [Path] -> Maybe(Direction)
findPath goal paths = let isPathToGoal (dir,_,_) = goal == dir

                          occAndSmell (_,mbocc,mbsmell) = (isJust mbocc, mbsmell)

                          better p1 p2 = case (occAndSmell p1, occAndSmell p2) of
                                              ((True, _), (False, _)) -> p2
                                              ((False, Just sm1), (False, Just sm2)) -> if sm1 >= sm2
                                                                                        then p1
                                                                                        else p2
                                              (_,_) -> p1

                          findPathIn goalP paths = foldl better goalP paths
                       in
                          case (filter isPathToGoal paths) of
                                [] -> Nothing
                                [goalP] -> case (findPathIn goalP paths) of
                                                 (dir, Nothing, _) -> Just dir
                                                 _ -> Nothing
