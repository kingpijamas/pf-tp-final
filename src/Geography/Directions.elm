module Geography.Directions where

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