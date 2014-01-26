module AntColony.Main where

import Window
import Dict
import AntColony.Model.Data.Terrain as T


import open AntColony.Geography.Area
import open AntColony.Geography.Direction

import open AntColony.Model.Data.AntT
import open AntColony.Model.Data.AntNestT
import open AntColony.Model.Data.Food

import open AntColony.Utils.MaybeMonad
import open AntColony.Utils.Tuple
import open AntColony.Utils.SignalFunction

import AntColony.Model.Perceiving
import AntColony.Model.LoadSensing
import AntColony.Model.Loading
import AntColony.Model.Moving
import AntColony.Model.Rotating
import AntColony.Model.Scenting
import AntColony.Model.Seeing
import AntColony.Model.Smelling
import AntColony.Model.Remembering

tileSize = 20

main = lift2 display Window.dimensions (foldp step simulation <| (fps 30))

simulation : T.Terrain
simulation = let
                 pos' occ ph = T.position (Just occ) ph
 
                 tiles = []
                       {--  [ ( (coords 1 1), pos' T.Rock Nothing )
                         , ( (coords 1 2), pos' (T.Ant ant) Nothing )
                         , ( (coords 2 2), pos' (T.Ant ant) Nothing )
                         , ( (coords 4 4), pos' (T.AntNest antNest) Nothing )
                         , ( (coords 4 1), pos' (T.FoodChunk (foodChunk 5)) Nothing )
                         ]
                         --}
             in 
                T.terrain 4 4 tiles

step : Float->T.Terrain->T.Terrain
step t terrain = terrain

display : (Int,Int) -> T.Terrain -> Element
display (w, h) terrain = collage w h <| terrainAsForm terrain

asTileList : T.Terrain -> [(Coords, T.Position)]
asTileList terrain = Dict.toList terrain.elems

terrainAsForm : T.Terrain -> [Form]
terrainAsForm terrain = 
    let 
        ground = terrainMatrixForms terrain.width terrain.height tileSize
        occupants = terrainTilesAsForm terrain
    in (ground ++ occupants)

-- Obtiene la lista de Elements de los tiles en la matriz a dibujar
terrainMatrixForms : Int -> Int -> Int -> [Form]
terrainMatrixForms width height tileSize = map (\y -> terrainRowForms width y tileSize) [1..height] |> concat

-- Obtiene la lista de Elements para los tiles en una fila a dibujar
terrainRowForms : Int -> Int -> Int -> [Form]
terrainRowForms width y tileSize = map (\x -> terrainSqareForm x y tileSize) [1..width]

-- Obtiene el Element que representa el pos a dibujar
terrainSqareForm : Int -> Int -> Int -> Form
terrainSqareForm x y tileSize = 
    let
        xOffsset = toFloat (x * tileSize)
        yOffsset = toFloat (y * tileSize)
    in squarePath (toFloat tileSize) |> traced (solid green) |> translateTile x y tileSize

translateTile : Int -> Int -> Int -> Form -> Form
translateTile x y tileSize form =
    let
        xOffsset = toFloat (x * tileSize)
        yOffsset = toFloat (y * tileSize)
    in move (xOffsset, yOffsset) form

squarePath : Float -> Path
squarePath len = let hlen = len / 2 in path [(-hlen, -hlen), (hlen, -hlen), (hlen, hlen), (-hlen,hlen), (-hlen, -hlen)]                          

terrainTilesAsForm : T.Terrain -> [Form]
terrainTilesAsForm terrain = map (\(position, pos) -> terrainTileForm position pos tileSize) <| asTileList terrain

terrainTileForm : Coords -> T.Position -> Int-> Form
terrainTileForm position pos tileSize = 
    case pos.occupant of
        Just (T.Rock) -> toForm stoneImg |> translateTile (getX position) (getY position) tileSize
        Just (T.Ant ant) -> toForm antImg |> translateTile (getX position) (getY position) tileSize
        Just (T.AntNest nest) -> toForm antNestImg |> translateTile (getX position) (getY position) tileSize
        Just (T.FoodChunk foodChunk) -> toForm foodChunkImg |> translateTile (getX position) (getY position) tileSize

antImg : Element
antImg = image 20 20 "resources/ant.png"

stoneImg : Element
stoneImg = image 20 20 "resources/stone.png"

antNestImg : Element
antNestImg = image 20 20 "resources/anthill.jpg"

foodChunkImg : Element
foodChunkImg = image 20 20 "resources/apple.jpg"