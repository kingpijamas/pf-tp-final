module Model.Ant.Scenting where

import Capacities.Scenting as Sc
import open Model.Terrain
import open Utils.MaybeMonad

type ScentF = (Tile -> Tile)

scent : ScentF
scent tile = { tile | scent <- tile.scent + 1 }

unscent : ScentF
unscent tile = { tile | scent <- tile.scent - 1 }

scentUnscent : ScentF -> Sc.ScentF Tile
scentUnscent sf area pos = let 
                               updateArea tile' = add area pos tile'
                            in
                               (area `get` pos)             -- : Maybe(Tile)
                                >>= (return . sf)           -- : Tile -> Maybe(Tile)
                                >>= updateArea              -- : Tile -> Maybe(Area a)

type Scenter = Sc.Scenter Tile

scenter : Terrain -> Scenter
scenter area = let scent' = scentUnscent scent
                   unscent' = scentUnscent unscent
                in
                   Sc.scenter scent' unscent' area