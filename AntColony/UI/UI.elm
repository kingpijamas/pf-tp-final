module AntColony.UI.UI where

import open AntColony.Geography.Coords
import open AntColony.Geography.Direction
import open AntColony.Model.Scent

import AntColony.Model.Terrain as T

tileSize = 20 -- px

display : (Int,Int) -> Maybe(T.Terrain) -> Element
display (w, h) mbterrain = case mbterrain of
                                Just terrain -> collage w h <| terrainAsForm terrain
                                _ -> asText "Oops, there was an error"

terrainAsForm : T.Terrain -> [Form]
terrainAsForm terrain = let ground = terrainMatrixForms terrain
                            occupants = terrainTilesAsForm terrain
                         in
                            {-(toForm . asText <| (T.getAnts terrain)) ::-}
                            (toForm . asText <| (T.getFood terrain)) :: (ground ++ occupants)

-- Obtiene la lista de Elements de los tiles en la matriz a dibujar
terrainMatrixForms : T.Terrain -> [Form]
terrainMatrixForms terrain = map (\y -> terrainRowForms terrain y tileSize) [1..(T.height terrain)] |> concat . concat

-- Obtiene la lista de Elements para los tiles en una fila a dibujar
terrainRowForms : T.Terrain -> Int -> Int -> [[Form]]
terrainRowForms terrain y tileSize = map (\x -> terrainSqareForm terrain x y tileSize)
                                         [1..(T.width terrain)]

offset : Int -> Float
offset x = toFloat (x * tileSize)

-- Obtiene el Element que representa el pos a dibujar
terrainSqareForm : T.Terrain -> Int -> Int -> Int -> [Form]
terrainSqareForm terrain x y tileSize = let pheromone x y = case (T.getScent terrain (coords x y)) of
                                                                 Just pos -> pos
                                                                 _ -> 0
                                         in
                                            [ squarePath (toFloat tileSize)
                                              |> traced (solid green)
                                              |> translateTile x y tileSize
                                            , intToForm (pheromone x y)
                                              |> move (offset x, offset y)
                                            ]

translateTile : Int -> Int -> Int -> Form -> Form
translateTile x y tileSize form = move (offset x, offset y) form

squarePath : Float -> Path
squarePath len = let hlen = len / 2
                 in
                    path [ (-hlen, -hlen)
                         , (hlen, -hlen)
                         , (hlen, hlen)
                         , (-hlen,hlen)
                         , (-hlen, -hlen)
                         ]

terrainTilesAsForm : T.Terrain -> [Form]
terrainTilesAsForm terrain = asTileList terrain
                             |> map (\(coord, tile) -> terrainTileForm coord tile tileSize)
                             |> concat

asTileList : T.Terrain -> [(Coords, T.Position)]
asTileList terrain = T.toList terrain

terrainTileForm : Coords -> T.Position -> Int -> [Form]
terrainTileForm coords tile tileSize = let x = getX coords
                                          
                                           y = getY coords

                                           translate form = translateTile x y tileSize form
                                        in
                                           [ toForm (getImage tile) 
                                              |> translate
                                              |> (rototateTile tile)
                                           , intToForm (getPheromone tile)
                                              |> translate
                                           ]

rototateTile : T.Position -> Form -> Form
rototateTile tile form = let angle tile = case tile.occupant of
                                               Just (T.Ant ant) -> directionToRadians (ant.orientation)
                                               _ -> 0
                          in
                             rotate (angle tile) form

directionToRadians : Direction -> Float
directionToRadians dir = let dPerRad = 0.01745

                             rads dir = case dir of
                                             SW -> 0
                                             S -> 45
                                             SE -> 90
                                             E -> 135
                                             NE -> 180
                                             N -> 225
                                             NW -> 270
                                             W -> 315
                          in 
                             dPerRad * (rads dir)

getImage : T.Position -> Element
getImage tile = case tile.occupant of
                     Just (T.Rock) -> stoneImg
                     Just (T.Ant ant) -> antImg
                     Just (T.AntNest nest) -> antNestImg
                     Just (T.FoodChunk foodChunk) -> foodChunkImg
                     _ -> asText "" -- TODO 

soilImg : Element
soilImg = tileImage "resources/floor.png"

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