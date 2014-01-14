module Geography.Direction where
import open Geography.Area

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
                    N   ->  Just (coords (-1) 0)
                    NE  ->  Just (coords (-1) 1)
                    E   ->  Just (coords 0 1)
                    SE  ->  Just (coords 1 1)
                    S   ->  Just (coords 1 0)
                    SW  ->  Just (coords 1 (-1))
                    W   ->  Just (coords 0 (-1))
                    NW  ->  Just (coords (-1) (-1))
                    _     ->  Nothing

addDir : Coords->Direction->Maybe(Coords)
addDir a dir = case (asCoords dir) of
                  Just b -> Just (addCoord a b)
                  Nothing -> Nothing