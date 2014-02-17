module AntColony.Main where

import Window
import Dict
import AntColony.Model.Terrain as T

import open AntColony.Geography.Coords
import open AntColony.Geography.Direction
import open AntColony.Utils.SignalFunction
import open AntColony.Utils.Tuple

import open AntColony.Model.AntT
import open AntColony.Model.AntNestT
import open AntColony.Model.Food

--TODO: TEST!
import open AntColony.Utils.Test

import AntColony.UI.UI as UI

import AntColony.Logic.Ant as Ant
import AntColony.Logic.Pheromone as Pheromone

width = 10
height = 12

main = lift2 UI.display Window.dimensions (loop step (Just simulation) (fps 1))

simulation : T.Terrain
simulation = let pos' occ = T.position (Just occ) Nothing

                 nestPos = coords 2 2

                 addNest position = (position, pos' (T.AntNest antNest))                 
                 addAnt position orientation = (position, pos' (T.Ant (ant nestPos position orientation)))
                 addFood position x = (position, pos' (T.FoodChunk (foodChunk x)))

                 tiles = [ addNest nestPos
                         --, addAnt (coords 2 2) N
                         , addAnt (coords 4 4) NE
                         --, addAnt (coords 5 5) N
                         --, addAnt (coords 6 6) E
                         --, addAnt (coords 7 7) W
                         , addFood (coords 8 8) 5
                         ] ++ (buildSurroundingStones width height)
              in 
                 T.terrain width height tiles

buildSurroundingStones : Int -> Int -> [(Coords, T.Position)]
buildSurroundingStones w h = let buildRock (x,y) = (coords x y, T.position (Just T.Rock) Nothing)
                              in 
                                 foldl (\coord list -> (buildRock coord) :: list) [] 
                                  <| (map (\y -> (1, y)) [1..h])
                                     ++ (map (\y -> (w, y)) [1..h])
                                     ++ (map (\x -> (x, 1)) [2..w - 1])
                                     ++ (map (\x -> (x, h)) [2..w - 1])

step : SF (Float, Maybe(T.Terrain)) (Maybe(T.Terrain))
step = (arr snd)                    -- : SF (Float, Maybe(T.Terrain)) (Maybe(T.Terrain))
       -- >>> (arr setWidthMF)
        -->>> (arr (moveMF (4,4) (5,5)))
        -->>> (arr (moveMF (5,5) (4,4)))
       -- >>> (arr (evictMF (2,2)))
       -- >>> (arr (occupyMF (3,3) T.Rock))
        >>> (Ant.animateAnts)       -- : SF (Maybe(T.Terrain)) (Maybe(T.Terrain))
       -- >>> (Pheromone.decayAll)    -- : SF (Maybe(T.Terrain)) (Maybe(T.Terrain))
