module AntColony.Main where

import Window
import AntColony.Model.Terrain as T
import AntColony.Utils.Matrix as M
{-
import AntColony.Geography.Area as A
import AntColony.Geography.Direction as D

import AntColony.Capacities.Moving as Mv
import AntColony.Capacities.Loading as L
import AntColony.Capacities.Scenting as Sc
import AntColony.Capacities.Perceiving



import AntColony.Model.Moving
import AntColony.Model.Scenting
import AntColony.Model.Seeing
import AntColony.Model.Smelling
import AntColony.Model.Loading
-}
{-
type Anthill = {ants:[Ant.Ant]}

ants : [Ant.Ant]
ants = [{food = Nothing , limit = Just 1}]

antMap : Dict.Dict Int Int
antMap = Dict.empty

area : A.Area a
area = {elems = Dict.empty, rows = 1, cols = 5}
-}

main = lift2 display Window.dimensions (foldp step simulation <| (fps 30))

simulation : T.Terrain
simulation = T.newTerrain

step : Float->T.Terrain->T.Terrain
step t terrain = terrain

display (w, h) terrain = 
    let tileSize = toFloat (T.tileSize terrain)
    in collage w h <| ((tilesCollage tileSize terrain) ++ (antsCollage tileSize terrain))

tilesCollage tileSize terrain = map (\n -> drawTile n tileSize) (T.getTiles terrain)
antsCollage tileSize terrain = map (\n -> drawAnt n tileSize) (T.getAnts terrain)

-- drawTile : M.Position -> Int -> Form      -- CUAL ES EL TIPO DE ESTO??
drawTile position len = 
    let
        xOffsset = toFloat (M.row position) * len
        yOffsset = toFloat (M.col position) * len
    in squarePath (len) |> traced (solid green) |> move (xOffsset, yOffsset) 

squarePath len = let hlen = len / 2 in path [(-hlen, -hlen), (hlen, -hlen), (hlen, hlen), (-hlen,hlen), (-hlen, -hlen)]

-- drawAnt : M.Position -> Int -> Form
drawAnt position len = 
    let
        xOffsset = toFloat (M.row position) * len
        yOffsset = toFloat (M.col position) * len
    in toForm antImg |> move (xOffsset, yOffsset)

antImg = image 20 20 "resources/ant.png"
-- Dibuja hormigas: map (\n -> toForm antImg) (T.getAnts theTerrain)





-- main = plainText "Hello, World!"

