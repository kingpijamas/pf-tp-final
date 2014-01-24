module AntColony.Model.Scenting where

import open AntColony.Model.Data.Terrain
import open AntColony.Geography.Area

import open AntColony.Model.Data.Scentable
import open AntColony.Utils.MaybeMonad

--import open AntColony.Utils.SignalFunction

data Action = Scent | Unscent

type ScentIntent = { target:Coords
                   , action:Action
                   }

scentFacade : Terrain -> ScentIntent -> Maybe(Terrain)
scentFacade terrain sig = let targetPos = sig.target

                              scf = case sig.action of
                                         Scent ->  (return . scent)
                                         Unscent -> unscent
                           in
                              scentUnscent scf terrain targetPos  -- Maybe(Terrain)


--type Scenter a = SF (ScentIntent) (Maybe(Terrain))

--scenter : (Terrain -> Coords -> Maybe(Terrain)) -> UnscentF a -> Terrain -> Scenter a
--scenter scent unscent terrain = arr (scentProxy scent unscent terrain)

--module AntColony.Capacities.Scenting where

scentUnscent : (Tile -> Maybe(Tile)) -> Terrain -> Coords -> Maybe(Terrain)
scentUnscent scf terrain pos = let updateArea tile' = add terrain pos tile'
                                in
                                   (terrain `get` pos)   -- : Maybe(Tile)
                                    >>= scf              -- : Tile -> Maybe(Tile)
                                    >>= updateArea       -- : Tile -> Maybe(Terrain)

--type Scenter = Sc.Scenter Tile -- : SF (ScentIntent) (Maybe(Terrain))

--scenter : Terrain -> Scenter
--scenter terrain = let scent' = scentUnscent scent
--                      unscent' = scentUnscent unscent
--                   in
--                      Sc.scenter scent' unscent' terrain