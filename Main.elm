module AntColony.Main where

import Window
import Dict
import Time as Time
import AntColony.Model.Terrain as T

import open AntColony.Geography.Coords
import open AntColony.Geography.Direction
import open AntColony.Utils.SignalFunction
import open AntColony.Utils.Tuple

import open AntColony.Model.AntT
import open AntColony.Model.AntNestT
import open AntColony.Model.FoodChunkT
import open AntColony.Model.Food

import AntColony.UI.UI as UI

import AntColony.Logic.Ant as Ant
import AntColony.Logic.Pheromone as Pheromone

width = 10
height = 10

main = lift2 UI.display Window.dimensions (loop step (Just simulation) (Time.every <| 500 * Time.millisecond))--(fps 1))

simulation : T.Terrain
simulation = let pos' occ = T.position (Just occ) Nothing

                 nestPos = coords 2 2

                 addPheromone position scent = (position, T.position Nothing (Just scent))
                 addNest position = (position, pos' (T.AntNest antNest))                 
                 addAnt position orientation = (position, pos' (T.Ant (ant nestPos position orientation)))
                 addFood position x = (position, pos' (T.FoodChunk (foodChunk x)))
                 addRock position = (position, pos' (T.Rock))

                 tiles = [ addNest nestPos
                         , addRock (coords 2 4)
                         , addFood (coords 4 8) 10
                         , addRock (coords 6 4)
                         , addFood (coords 8 4) 5 
                         , addRock (coords 2 6)
                         , addRock (coords 4 6)
                         , addRock (coords 6 6)
                         , addFood (coords 8 6) 100
                         , addRock (coords 2 9)
                         , addRock (coords 4 9)
                         , addRock (coords 6 9)
                         , addFood (coords 8 9) 1000
                         , addFood (coords 6 2) 5
                         --, addAnt (coords 4 2) N
                         --, addAnt (coords 5 2) N
                         --, addAnt (coords 2 3) N
                         --, addAnt (coords 3 2) E
                         , addAnt (coords 2 3) SE
                         --, addAnt (coords 2 5) SW

                         --, addAnt (coords 3 4) N
                         --, addFood (coords 2 5) 400
                         --, addFood (coords 3 5) 100
                         --, addFood (coords 4 5) 200
                         --, addFood (coords 9 4) 100
                         --, pheromone (coords 8 8) 10
                         --, pheromone (coords 3 3) 10
                         --, pheromone (coords 4 4) 10
                         --, scent (coords 5 5) 10
                         --, scent (coords 3 5) 5
                         --, ant (coords (width-1) 3) W
                         --, ant (coords 4 5) E
                         --, ant (coords 4 6) E
                         --, ant (coords 5 6) S
                         --, ant (coords 6 6) S
                         --, ant (coords 6 5) W
                         --, ant (coords 6 4) W
                         --, ant (coords 5 4) N

                         --, ant (coords 5 5) N
                         --, ant (coords 6 6) E
                         --, ant (coords 3 3) W
                         ] ++ surroundingStones 
                           --++ (makeAnts ants)
                           --++ (makeFoods foods)
              in 
                 T.terrain width height tiles

surroundingStones : [(Coords, T.Position)]
surroundingStones = let buildRock (x,y) = (coords x y, T.position (Just T.Rock) Nothing)
                     in 
                        foldl (\coord list -> (buildRock coord) :: list) [] (rect (width,height))

rect : Coords -> [Coords]
rect (w,h) = (map (\y -> (1, y)) [1..h])
              ++ (map (\y -> (w, y)) [1..h])
              ++ (map (\x -> (x, 1)) [2..w - 1])
              ++ (map (\x -> (x, h)) [2..w - 1])

step : SF (a, Maybe(T.Terrain)) (Maybe(T.Terrain))
step = (arr snd)                    -- : SF (a, Maybe(T.Terrain)) (Maybe(T.Terrain))
         >>> (Ant.animateAnts)      -- : SF (Maybe(T.Terrain)) (Maybe(T.Terrain))
         >>> (Pheromone.decayAll)   -- : SF (Maybe(T.Terrain)) (Maybe(T.Terrain))