module AntColony.Main where

import Window
import Dict
import AntColony.Model.Terrain as T

import open AntColony.Geography.Area
import open AntColony.Geography.Direction

import open AntColony.Model.AntT
import open AntColony.Model.AntNestT
import open AntColony.Model.Food

import AntColony.Logic.AntLogic

tileSize = 20
width = 10
height = 12

main = lift2 display Window.dimensions (foldp step simulation <| (fps 30))

simulation : T.Terrain
simulation = let pos' occ = T.position (Just occ) Nothing
                 
                 tiles = [ (coords 3 3, pos' (T.Ant ant))
                         , (coords 2 2, pos' (T.Ant ant))
                         , (coords 4 4, pos' (T.AntNest antNest))
                         , (coords 9 10, pos' (T.FoodChunk (foodChunk 5)))
                         ] ++ (buildSurroundingStones width height)
              in 
                 T.terrain width height tiles

buildSurroundingStones : Int -> Int -> [(Coords, T.Position)]
buildSurroundingStones w h = let buildRock (x,y) = (coords x y, T.position (Just T.Rock) Nothing)
                              in 
                                 foldl (\coord list -> (buildRock coord) :: list) [] 
                                  <| 
                                     (map (\y -> (1, y)) [1..h])
                                     ++ (map (\y -> (w, y)) [1..h])
                                     ++ (map (\x -> (x, 1)) [2..w - 1])
                                     ++ (map (\x -> (x, h)) [2..w - 1])

step : Float->T.Terrain->T.Terrain
step t terrain = signalAnts <| terrain 

--TODO: mover esto a Behaviour (o donde tenga que ir...)
signalAnts : T.Terrain -> T.Terrain
signalAnts terrain = foldl signalAnt terrain <| (T.getAnts terrain)

signalAnt : T.Occupant -> T.Terrain -> T.Terrain
signalAnt ant terrain = terrain
-- ============================================

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
terrainTileForm position pos tileSize = toForm (getImage pos) |> translateTile (getX position) (getY position) tileSize

getImage : T.Position -> Element
getImage pos = 
    case pos.occupant of
        Just (T.Rock) -> stoneImg
        Just (T.Ant ant) -> antImg
        Just (T.AntNest nest) -> antNestImg
        Just (T.FoodChunk foodChunk) -> foodChunkImg

antImg : Element
antImg = image 20 20 "resources/ant.png"

stoneImg : Element
stoneImg = image 20 20 "resources/stone.png"

antNestImg : Element
antNestImg = image 20 20 "resources/anthill.jpg"

foodChunkImg : Element
foodChunkImg = image 20 20 "resources/apple.jpg"

