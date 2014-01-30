module AntColony.Utils.List where

flatZip : [a] -> [(b,c)] -> [(a,b,c)]
flatZip xs yzs = zipWith (\x (y,z) -> (x,y,z)) xs yzs