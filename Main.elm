module AntColony.Main where

import Window
import AntColony.Model.Terrain as T

import AntColony.Geography.Area as A
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
        ground = terrainMatrixForms terrain.tiles.width terrain.tiles.height terrain.tileSize
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
terrainTilesAsForm terrain = map (\(position, tile) -> terrainTileForm position tile terrain.tileSize) <| T.asTileList terrain

terrainTileForm : A.Coords -> T.Tile -> Int-> Form
terrainTileForm position tile tileSize = 
    case tile.occupant of
        Just (T.RockTile) -> toForm stoneImg |> translateTile (A.getX position) (A.getY position) tileSize
        Just (T.AntTile ant) -> toForm antImg |> translateTile (A.getX position) (A.getY position) tileSize

antImg : Element
antImg = image 20 20 "resources/ant.png"

stoneImg : Element
stoneImg = image 20 20 "resources/stone.png"
