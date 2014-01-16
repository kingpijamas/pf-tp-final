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

main = lift2 display Window.dimensions (foldp step simulation <| (fps 30))

simulation : T.Terrain
simulation = T.newTerrain

step : Float->T.Terrain->T.Terrain
step t terrain = terrain

display : (Int,Int) -> T.Terrain -> Element
display (w, h) terrain = collage w h <| terrainAsForm terrain

terrainAsForm : T.Terrain -> [Form]
terrainAsForm terrain = 
    let 
        ground = terrainMatrixForms terrain.tiles.rows terrain.tiles.cols terrain.tileSize
        occupants = terrainTilesAsForm terrain
    in (ground ++ occupants)

-- Obtiene la lista de Elements de los tiles en la matriz a dibujar
terrainMatrixForms : Int -> Int -> Int -> [Form]
terrainMatrixForms rows columns tileSize = map (\row -> terrainRowForms row columns tileSize) [1..rows] |> concat

-- Obtiene la lista de Elements para los tiles en una fila a dibujar
terrainRowForms : Int -> Int -> Int -> [Form]
terrainRowForms row columns tileSize = map (\column -> terrainSqareForm row column tileSize) [1..columns]

-- Obtiene el Element que representa el tile a dibujar
terrainSqareForm : Int -> Int -> Int -> Form
terrainSqareForm row column tileSize = 
    let
        xOffsset = toFloat (row * tileSize)
        yOffsset = toFloat (column * tileSize)
    in squarePath (toFloat tileSize) |> traced (solid green) |> translateTile row column tileSize

translateTile : Int -> Int -> Int -> Form -> Form
translateTile row column tileSize form =
    let
        xOffsset = toFloat (row * tileSize)
        yOffsset = toFloat (column * tileSize)
    in move (xOffsset, yOffsset) form

squarePath : Float -> Path
squarePath len = let hlen = len / 2 in path [(-hlen, -hlen), (hlen, -hlen), (hlen, hlen), (-hlen,hlen), (-hlen, -hlen)]                          

terrainTilesAsForm : T.Terrain -> [Form]
terrainTilesAsForm terrain = map (\(position, tile) -> terrainTileForm position tile terrain.tileSize) <| T.asTileList terrain

terrainTileForm : M.Position -> T.Tile -> Int-> Form
terrainTileForm position tile tileSize = 
    case tile.occupant of
        Just (T.RockTile) -> toForm stoneImg |> translateTile (M.row position) (M.col position) tileSize
        Just (T.AntTile ant) -> toForm antImg |> translateTile (M.row position) (M.col position) tileSize
        Just (T.AntNestTile nest) -> toForm antNestImg |> translateTile (M.row position) (M.col position) tileSize
        Just (T.FoodTile foodChunk) -> toForm foodChunkImg |> translateTile (M.row position) (M.col position) tileSize

antImg : Element
antImg = image 20 20 "resources/ant.png"

stoneImg : Element
stoneImg = image 20 20 "resources/stone.png"

antNestImg : Element
antNestImg = image 20 20 "resources/anthill.jpg"

foodChunkImg : Element
foodChunkImg = image 20 20 "resources/apple.jpg"

