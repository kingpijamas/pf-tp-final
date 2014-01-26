module AntColony.Geography.Direction where

import open AntColony.Geography.Area

data Direction = N | NE | E | SE | S | SW | W | NW

lft : Direction -> Direction
lft d = case d of
             N   ->  NW
             NW  ->  W
             W   ->  SW
             SW  ->  S
             S   ->  SE
             SE  ->  E
             E   ->  NE
             NE  ->  N

rght : Direction -> Direction
rght d = case d of
              N   ->  NE
              NE  ->  E
              E   ->  SE
              SE  ->  S
              S   ->  SW
              SW  ->  W
              W   ->  NW
              NW  ->  N

asCoords : Direction -> Maybe(Coords)
asCoords dir = case dir of
                    N   ->  Just (coords 0 1)
                    NE  ->  Just (coords 1 1)
                    E   ->  Just (coords 1 0)
                    SE  ->  Just (coords 1 (-1))
                    S   ->  Just (coords 0 (-1))
                    SW  ->  Just (coords (-1) (-1))
                    W   ->  Just (coords (-1) 0)
                    NW  ->  Just (coords (-1) 1)
                    _     ->  Nothing


dirTo : Coords -> Coords -> Direction
dirTo from to = let vers x = if x == 0
                             then x
                             else abs(x)

                    subt = to `subtCoord` from

                    asDir (x,y) = case (x,y) of
                                       (0,1)       -> N
                                       (1,1)       -> NE
                                       (1,0)       -> E
                                       (1,(-1))    -> SE
                                       (0,(-1))    -> S
                                       ((-1),(-1)) -> SW
                                       ((-1),0)    -> W
                                       ((-1),1)    -> NW
                 in
                    asDir (vers (getX subt),vers (getY subt))


addDir : Coords -> Direction -> Maybe(Coords)
addDir a dir = case (asCoords dir) of
                  Just b -> Just (addCoord a b)
                  Nothing -> Nothing

lftN : Direction -> Int -> Direction
lftN dir times = case times of
                    0 -> dir
                    n -> lftN (lft dir) (n-1)

rghtN : Direction -> Int -> Direction
rghtN dir times = case times of
                    0 -> dir
                    n -> rghtN (rght dir) (n-1)
