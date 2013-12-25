module Area where
import src/utils/Matrix.elm as M
import src/CardinalDirs.elm as C

type Area a = M.Matrix a

type Coords = M.Position

coords = position

getX:Coords->Int
getX loc = loc.row

getY:Coords->Int
getY loc = loc.col

(+):Coords->(Either Coords C.CardinalDirs)->Maybe(Coords)
(+) a b = let
             b' = case b of
                       Left coord   ->  coord
                       Right dir    ->  asCoords dir
          in
             case b' of
                Nothing     ->  Nothing
                Just coord  ->  coords ((getX a)+(getX b)) ((getY a)+(getY b))

asCoords:C.CardinalDirs->Maybe(Coords)
asCoords dir = case dir of
                    North   ->  Just (coords (-1) 0)
                    East    ->  Just (coords 0 1)
                    South   ->  Just (coords 1 0)
                    West    ->  Just (coords 0 (-1))
                    _       ->  Nothing

type Locatable a = { a | area:Area a, location:Coords }