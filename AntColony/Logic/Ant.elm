module AntColony.Logic.Ant where

import open AntColony.Utils.List
import open AntColony.Utils.Tuple
import open AntColony.Utils.Maybe
import open AntColony.Utils.SignalFunction

import open AntColony.Geography.Coords
import open AntColony.Geography.Direction

import open AntColony.Model.Terrain
import open AntColony.Model.Scent
import open AntColony.Model.AntT
--import open AntColony.Model.Food
import open AntColony.Model.FoodCarrier

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

type SensorData = ([Maybe(Sight)], [Maybe(Smell)], Maybe(LoadStatus))

getSensingDirs : Direction -> [Direction]
getSensingDirs orientation = [orientation, lft orientation, rght orientation]

sense : SF (Terrain, AntT) SensorData
sense = let dirPerceptors pf (terrain, ant) = perceiveInDirs pf (getSensingDirs ant.orientation) terrain ant.position
            eyes = arr (dirPerceptors see)                    -- : SF (Terrain, AntT) [Maybe(Sight)]
            antennae = arr (dirPerceptors smell)              -- : SF (Terrain, AntT) [Maybe(Smell)]

            perceptor pf (terrain, ant) = perceive pf terrain ant.position
            loadSensor  =  arr (perceptor senseLoad)         -- : SF (Terrain, AntT) Maybe(LoadStatus)
         in
            (eyes &&& antennae &&& loadSensor) >>^ (flatten)  -- : SF (Terrain, AntT) (SensorData)

act : ((Terrain, AntT), SensorData) -> Maybe(Terrain)
act ((terrain,ant),(seen,smelled,loadStatus)) =
    let currPos = ant.position
        forward = ant.orientation
        toNest = currPos `dirTo` ant.nestPos

        turn times = clckN times terrain currPos
        turnTo goal = turn (turnsTo forward goal rght)

        walk scenting goal = if goal /= forward
                             then turnTo goal
                             else (if not scenting
                                   then moveInDir terrain currPos goal
                                   else (scent terrain currPos) 
                                         >>= (\terr -> moveInDir terr currPos goal))

        loadFrom path = loadInDir terrain currPos (getDir path)
        unloadTo path terrain = unloadInDir terrain currPos (getDir path)

        getDir (dir,_,_) = dir
        getSmell (_, _, mbsmell) = mbsmell

        hasFood (_,mbocc,_) = case mbocc of
                                   Just(FoodChunk _) -> True
                                   _ -> False
        hasNest (_,mbocc,_) = case mbocc of
                                   Just(AntNest _) -> True
                                   _ -> False

        isEmpty (_, mbsth, _) = isNothing mbsth
        
        hasPheromone (_,_,mbph) = isJust mbph

        smellier p1 p2 = case (getSmell p1, getSmell p2) of
                              (Just sm1, Just sm2) -> if sm1 >= sm2 then p1 else p2
                              (_,_) -> p1

        --type Path = (Direction, Maybe(Sight), Maybe(Smell))                                      

        sensed = asPaths forward seen smelled
        
        foodPs = filter hasFood sensed
        emptyPs = filter isEmpty sensed
        nestPs = filter hasNest sensed

        smelliestP ps = case filter hasPheromone ps of
                             [] -> Nothing
                             _  -> return (foldr1 smellier ps)

     in
        case loadStatus of
             Just (Empty) -> case (foodPs, smelliestP emptyPs, emptyPs) of
                                  ((fp::fps), _, _)    -> loadFrom fp
                                  (_, Just smp, _)     -> walk False <| getDir smp
                                  (_, _, (ep::eps))    -> walk False <| getDir ep
                                  (_, _, _)            -> turn 1
             Just (Full) -> case (smelliestP emptyPs, nestPs, emptyPs) of
                                 (Just smp, _, _)      -> walk True <| getDir smp
                                 (_, (np::nps), _)     -> (scent terrain currPos) >>= (unloadTo np)
                                 (_, _, (ep::eps))     -> walk True <| toNest 
                                 (_, _, _)             -> turn 1



--towardsDo goal dirF = case (findPath goal (asPaths forward seen smelled)) of
--                           Just dir -> dirF dir
--                           Nothing -> turn 2
--case (head seen, head smelled, loadStatus) of
--     (Just (FoodChunk _), _, Just(Empty))    -> front >>= loadFrom
--     (Just (FoodChunk _), _, Just(Full))     -> turnAround -- could probably be removed
--     (Just (AntNest _), _, Just (Full))      -> front >>= unloadTo
--     (Just (AntNest _), _, Just(Empty))      -> turnAround -- could probably be removed
--     (Just _, _, _) -> turn 1
--     (_, Nothing, Just cargo) -> towardsDo toNest (walk True)
--     (_, Just ph, Just cargo) -> towardsDo forward (walk True)
--     (_, Just ph, Nothing) -> towardsDo forward (walk False)
--     (_, _, _) -> walk False forward


type Path = (Direction, Maybe(Sight), Maybe(Smell))

asPaths : Direction -> [Maybe(Sight)] -> [Maybe(Smell)] -> [Path]
asPaths forward sight smell = flatZip (getSensingDirs forward) (zip sight smell)

findPath : Direction -> [Path] -> Maybe(Direction)
findPath goal paths = let freePaths = filter isEmpty paths
                          
                          isEmpty (_, mbsth, _) = isNothing mbsth

                          smell (_, _, mbsmell) = mbsmell

                          better p1 p2 = case (smell p1, smell p2) of
                                              (Just sm1, Just sm2) -> if sm1 >= sm2 then p1 else p2
                                              (_,_) -> p1

                          getDirection (dir, _, _) = dir
                       in
                          case freePaths of
                              [] -> Nothing
                              _ -> foldl better (head freePaths) freePaths |> return . getDirection
