module AntColony.Model.Scenting where

import AntColony.Capacities.Scenting as Sc
import open AntColony.Model.Terrain
import open AntColony.Utils.MaybeMonad

type ScentF = Tile -> Maybe(Tile)

scent : ScentF
scent tile = let scent' = case tile.scent of
                            Nothing -> return 1
                            Just scnt -> return (scnt + 1)
              in 
                 return { tile | scent <- scent' }

unscent : ScentF
unscent tile = let unscent' scnt = if scnt > 0
                                   then return { tile | scent <- return scnt }
                                   else Nothing
                in 
                   (tile.scent)     -- : Maybe(Pheromone)
                    >>= (unscent')  -- : Pheromone -> Maybe(Tile)

scentUnscent : ScentF -> Sc.ScentF Tile
scentUnscent sf area pos = let 
                               updateArea tile' = add area pos tile'
                            in
                               (area `get` pos)             -- : Maybe(Tile)
                                >>= (return . sf)           -- : Tile -> Maybe(Tile)
                                >>= updateArea              -- : Tile -> Maybe(Area a)

type Scenter = Sc.Scenter Tile -- : Automaton (ScentSignal) (Maybe(Terrain))

scenter : Terrain -> Scenter
scenter area = let scent' = scentUnscent scent
                   unscent' = scentUnscent unscent
                in
                   Sc.scenter scent' unscent' area