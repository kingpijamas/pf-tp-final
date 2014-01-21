module AntColony.Main where

import Window
import Dict
import AntColony.Model.Terrain as T

import open AntColony.Geography.Area
import open AntColony.Model.Ant.Ant
import open AntColony.Model.AntNest
import open AntColony.Model.FoodChunk

--import AntColony.Geography.Direction as D

--import AntColony.Capacities.Moving as Mv
--import AntColony.Capacities.Loading as L
--import AntColony.Capacities.Scenting as Sc
--import AntColony.Capacities.Perceiving



--import AntColony.Model.Moving
--import AntColony.Model.Scenting
--import AntColony.Model.Seeing
--import AntColony.Model.Smelling
--import AntColony.Model.Loading

tileSize = 20

main = lift2 display Window.dimensions (foldp step simulation <| (fps 30))

simulation : T.Terrain
simulation = let
                 tile' occ ph = T.tile (Just occ) ph

                 tiles = [ ( (coords 1 1), tile' T.RockTile Nothing )
                         , ( (coords 1 2), tile' (T.AntTile ant) Nothing )
                         , ( (coords 2 2), tile' (T.AntTile ant) Nothing )
                         , ( (coords 4 4), tile' (T.AntNestTile antNest) Nothing )
                         , ( (coords 4 1), tile' (T.FoodTile (foodChunk 5)) Nothing )
                         ]
              in 
                 T.terrain 4 4 tiles

step : Float->T.Terrain->T.Terrain
step t terrain = terrain

display : (Int,Int) -> T.Terrain -> Element
display (w, h) terrain = collage w h <| terrainAsForm terrain

asTileList : T.Terrain -> [(Coords, T.Tile)]
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

-- Obtiene el Element que representa el tile a dibujar
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
terrainTilesAsForm terrain = map (\(position, tile) -> terrainTileForm position tile tileSize) <| asTileList terrain

terrainTileForm : Coords -> T.Tile -> Int-> Form
terrainTileForm position tile tileSize = 
    case tile.occupant of
        Just (T.RockTile) -> toForm stoneImg |> translateTile (getX position) (getY position) tileSize
        Just (T.AntTile ant) -> toForm antImg |> translateTile (getX position) (getY position) tileSize
        Just (T.AntNestTile nest) -> toForm antNestImg |> translateTile (getX position) (getY position) tileSize
        Just (T.FoodTile foodChunk) -> toForm foodChunkImg |> translateTile (getX position) (getY position) tileSize

antImg : Element
antImg = image 20 20 "resources/ant.png"

stoneImg : Element
stoneImg = image 20 20 "resources/stone.png"

antNestImg : Element
antNestImg = image 20 20 "resources/anthill.jpg"

foodChunkImg : Element
foodChunkImg = image 20 20 "resources/apple.jpg"