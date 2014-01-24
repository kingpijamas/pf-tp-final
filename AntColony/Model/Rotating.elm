module AntColony.Model.Rotating where

import open AntColony.Utils.MaybeMonad

import open AntColony.Model.Data.Terrain

import open AntColony.Capacities.Positioning

import open AntColony.Geography.Area
import open AntColony.Geography.Direction

--import open AntColony.Utils.SignalFunction

data RotationSense = Clockwise | Counterclockwise

type RotationIntent = { from:Coords
                      , sense:RotationSense
                      }

rotationIntent : Coords -> RotationSense -> RotationIntent
rotationIntent from sense = { from=from
                            , sense=sense
                            }

rotateClockwise : Coords -> RotationIntent
rotateClockwise from = rotationIntent from Clockwise

rotateCounterclockwise : Coords -> RotationIntent
rotateCounterclockwise from = rotationIntent from Counterclockwise



rotationFacade : Terrain -> RotationIntent -> Maybe(Terrain)
rotationFacade terrain sig = let rf = case sig.sense of
                                           Clockwise -> clck
                                           Counterclockwise -> cntrclck
                              in
                                 rf terrain (sig.from)


rotate : (Direction -> Direction) -> Terrain -> Coords -> Maybe(Terrain)
rotate rf terrain pos = let rotateOcc tile = case tile.occupant of
                                                  Just(AntTile ant) -> setOccupant' tile (asAnt (rotate' ant))
                                                  _ -> Nothing

                            setOccupant' tile occ = return (setOccupant tile (Just occ))

                            rotate' rot = { rot | orientation <- (rf rot.orientation) }

                            updateTerrain tile' = add terrain pos tile'
                         in 
                            (terrain `get` pos)   -- : Maybe(Tile)
                             >>= (rotateOcc)      -- : Tile -> Maybe(Tile)
                             >>= (updateTerrain)  -- : Tile -> Maybe(Terrain)

clck : Terrain -> Coords -> Maybe(Terrain)
clck = rotate rght

cntrclck : Terrain -> Coords -> Maybe(Terrain)
cntrclck = rotate lft


--type Rotor a =  SF (RotationIntent) (Maybe(Area a))

--rotor : (RotationF a) -> (RotationF a) -> (Area a) -> (Rotor a)
--rotor clck cntrclck terrain = arr (rotationProxy clck cntrclck terrain)

--type Rotor = Rt.Rotor Tile -- : SF (DirectionIntent) (Maybe(Terrain))

--rotor : Terrain -> Rotor
--rotor terrain = Rt.rotor clck cntrclck terrain