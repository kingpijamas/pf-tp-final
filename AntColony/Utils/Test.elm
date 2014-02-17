module AntColony.Utils.Test where

import open AntColony.Utils.List
import open AntColony.Utils.Tuple
import open AntColony.Utils.Maybe
import open AntColony.Utils.SignalFunction

import open AntColony.Geography.Coords
import open AntColony.Geography.Direction

import open AntColony.Model.Terrain
import open AntColony.Model.Scent
import open AntColony.Model.AntT

import open AntColony.Logic.Perceiving
import open AntColony.Logic.LoadSensing
import open AntColony.Logic.Loading
import open AntColony.Logic.Moving
import open AntColony.Logic.Rotating
import open AntColony.Logic.Scenting
import open AntColony.Logic.Seeing
import open AntColony.Logic.Smelling

import open AntColony.Model.AntT
import open AntColony.Model.AntNestT
import open AntColony.Model.Food
import open AntColony.Model.Scent

--step : SF (Float, Maybe(T.Terrain)) (Maybe(T.Terrain))
--step = (arr snd)                    -- : SF (Float, Maybe(T.Terrain)) (Maybe(T.Terrain))
--        >>> 

setWidthMF : (Maybe(Terrain)) -> (Maybe(Terrain))
setWidthMF mbterr = mbterr
                     >>= (\terrain -> return (terrainMF 15 (height terrain) (toList terrain)))


--move : Terrain -> Coords -> Coords -> Maybe(Terrain)
moveMF : Coords -> Coords -> (Maybe(Terrain)) -> (Maybe(Terrain))
moveMF from to mbterr = let move' terrain = move terrain from to
                         in
                            mbterr              -- : Maybe(Terrain)
                             >>= (move')        -- : Terrain -> Maybe(Terrain,Occupant)

--evict : Terrain -> Coords -> Maybe(Terrain, Occupant)
evictMF : Coords -> (Maybe(Terrain)) -> (Maybe(Terrain))
evictMF coords mbterr = let evict' terrain = evict terrain coords
                         in
                            mbterr              -- : Maybe(Terrain)
                             >>= (evict')       -- : Terrain -> Maybe(Terrain,Occupant)
                             >>= (return . fst) -- : (Terrain, Occupant) -> Maybe(Terrain)


--occupy : Terrain -> Coords -> Occupant -> Maybe(Terrain)
occupyMF : Coords -> Occupant -> (Maybe(Terrain)) -> (Maybe(Terrain))
occupyMF whr occ mbterr = let occupy' terrain = occupy terrain whr occ
                           in
                              mbterr
                               >>= occupy'



--load : Terrain -> Coords -> Coords -> Maybe(Terrain)
loadMF : Coords -> Coords -> (Maybe(Terrain)) -> (Maybe(Terrain))
loadMF ldrPos unldrPos mbterr = let load' terrain = load terrain ldrPos unldrPos
                                 in
                                    mbterr
                                     >>= load'

--unload : Terrain -> Coords -> Coords -> Maybe(Terrain)
unloadMF : Coords -> Coords -> (Maybe(Terrain)) -> (Maybe(Terrain))
unloadMF unldrPos ldrPos mbterr = let unload' terrain = unload terrain unldrPos ldrPos
                                   in
                                      mbterr
                                       >>= unload'

--ld : Terrain -> Coords -> Food -> Maybe(Terrain, Maybe(Food))
ldMF : Coords -> Food -> (Maybe(Terrain)) -> (Maybe(Terrain))
ldMF ldrCoords fd mbterr = let ld' terrain = ld terrain ldrCoords fd
                            in
                               mbterr
                                >>= ld'
                                >>= (return . fst)

--unld : Terrain -> Coords -> Maybe(Terrain, Food)
unldMF : Coords -> (Maybe(Terrain)) -> (Maybe(Terrain))
unldMF unldrPos mbterr = let unld' terrain = unld terrain unldrPos
                          in
                             mbterr
                              >>= unld'
                              >>= (return . fst)
