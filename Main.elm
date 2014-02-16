module AntColony.Main where

import Window
import Dict
import AntColony.Model.Terrain as T

import open AntColony.Geography.Coords
import open AntColony.Geography.Coords
import open AntColony.Geography.Coords
import open AntColony.Geography.Direction
import open AntColony.Utils.SignalFunction
import open AntColony.Utils.Tuple

import open AntColony.Model.AntT
import open AntColony.Model.AntNestT
import open AntColony.Model.Food
import open AntColony.Model.Scent

import AntColony.Logic.Ant as Ant
--import AntColony.Logic.Pheromone as Pheromone

tileSize = 20
width = 10
height = 12

main = lift2 display Window.dimensions (loop step (Just simulation) <| (fps 30))

simulation : T.Terrain
simulation = let pos' occ = T.position (Just occ) Nothing
                
                 nestPos = coords 4 4

                 addNest position = (position, pos' (T.AntNest antNest))
                 
                 addAnt position orientation = (position, pos' (T.Ant (ant nestPos position orientation)))
                 
                 addFood position x = (position, pos' (T.FoodChunk (foodChunk x)))

                 tiles = [ addNest nestPos
                         --, addAnt (coords 2 2) N
                         , addAnt (coords 3 3) S
                         , addFood (coords 5 5) 5
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

step : SF (Float, Maybe(T.Terrain)) (Maybe(T.Terrain))
step = (arr snd)                    -- : SF (Float, Maybe(T.Terrain)) (Maybe(T.Terrain))
        >>> (Ant.animateAnts)       -- : SF (Maybe(T.Terrain)) (Maybe(T.Terrain))
        -- >>> (Pheromone.decayAll)    -- : SF (Maybe(T.Terrain)) (Maybe(T.Terrain))

display : (Int,Int) -> Maybe(T.Terrain) -> Element
display (w, h) mbterrain = case mbterrain of
                                Just terrain -> collage w h <| terrainAsForm terrain
                                _ -> asText "Oops, there was an error"

terrainAsForm : T.Terrain -> [Form]
terrainAsForm terrain = 
    let 
        ground = terrainMatrixForms terrain
        occupants = terrainTilesAsForm terrain
    in (ground ++ occupants)

-- Obtiene la lista de Elements de los tiles en la matriz a dibujar
terrainMatrixForms : T.Terrain -> [Form]
terrainMatrixForms terrain = map (\y -> terrainRowForms terrain y tileSize) [1..(T.height terrain)] |> concat . concat

-- Obtiene la lista de Elements para los tiles en una fila a dibujar
terrainRowForms : T.Terrain -> Int -> Int -> [[Form]]
terrainRowForms terrain y tileSize = 
    map (\x -> terrainSqareForm terrain x y tileSize) [1..(T.width terrain)]

-- Obtiene el Element que representa el pos a dibujar
terrainSqareForm : T.Terrain -> Int -> Int -> Int -> [Form]
terrainSqareForm terrain x y tileSize = 
    let
        xOffsset = toFloat (x * tileSize)
        yOffsset = toFloat (y * tileSize)
        pheromone x y = case (T.getScent terrain (coords x y)) of
            Just pos -> pos
            _ -> 0
    in [squarePath (toFloat tileSize) |> traced (solid green) |> translateTile x y tileSize,
         intToForm (pheromone x y) |> move (xOffsset, yOffsset)
       ]

translateTile : Int -> Int -> Int -> Form -> Form
translateTile x y tileSize form =
    let
        xOffsset = toFloat (x * tileSize)
        yOffsset = toFloat (y * tileSize)
    in move (xOffsset, yOffsset) form

squarePath : Float -> Path
squarePath len = let hlen = len / 2 in path [(-hlen, -hlen), (hlen, -hlen), (hlen, hlen), (-hlen,hlen), (-hlen, -hlen)]

terrainTilesAsForm : T.Terrain -> [Form]
terrainTilesAsForm terrain = asTileList terrain |> map (\(coord, tile) -> terrainTileForm coord tile tileSize) |> concat

asTileList : T.Terrain -> [(Coords, T.Position)]
asTileList terrain = T.toList terrain

terrainTileForm : Coords -> T.Position -> Int -> [Form]
terrainTileForm position tile tileSize = 
    let 
        x = getX position
        y = getY position
        translate form = translateTile x y tileSize form
    in
        [toForm (getImage tile) |> translate |> (rototateTile tile), intToForm (getPheromone tile) |> translate]

rototateTile : T.Position -> Form -> Form
rototateTile tile form = 
    let angle tile = case tile.occupant of
        Just (T.Ant ant) -> directionToRadians (ant.orientation)
        _ -> 0
    in
        rotate (angle tile) form

directionToRadians : Direction -> Float
directionToRadians dir = 0.01745 * (case dir of
    NE -> 315
    E -> 270
    SE -> 225
    S -> 180
    SW -> 135
    W -> 90
    NW -> 45
    N -> 0)

getImage : T.Position -> Element
getImage tile = 
    case tile.occupant of
        Just (T.Rock) -> stoneImg
        Just (T.Ant ant) -> antImg
        Just (T.AntNest nest) -> antNestImg
        Just (T.FoodChunk foodChunk) -> foodChunkImg

antImg : Element
antImg = tileImage "resources/ant.png"

stoneImg : Element
stoneImg = tileImage "resources/stone.png"

antNestImg : Element
antNestImg = tileImage "resources/anthill.jpg"

foodChunkImg : Element
foodChunkImg = tileImage "resources/apple.jpg"

tileImage : String -> Element
tileImage resource = image 20 20 resource

intToForm : Int -> Form
intToForm v = (toForm . text . toText . show) v

