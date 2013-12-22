module CardinalDirs where

data Direction = North | East | South | West

lft : Direction -> Direction
lft d = case d of
			North	->	West
			West	->	South
			South	->	East
			East	->	North

rght : Direction -> Direction
rght d = case d of
			North	->	East
			East	->	South
			South	->	West
			West	->	North