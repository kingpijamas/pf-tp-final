module Area where
import src/utils/Matrix.elm as Matrix
import src/geography/Directions.elm as Dirs

type Area a = Matrix.Matrix a

type Coords = Matrix.Position
coords = position

getX:Coords->Int
getX loc = loc.row

getY:Coords->Int
getY loc = loc.col

(+):Coords->(Either Coords Dirs.Directions)->Maybe(Coords)
(+) a b = let
             b' = case b of
                       Left coord   ->  coord
                       Right dir    ->  asCoords dir
          in
            case b' of
                Nothing     ->  Nothing
                Just coord  ->  coords ((getX a)+(getX b)) ((getY a)+(getY b))

asCoords:Dirs.Directions->Maybe(Coords)
asCoords dir = case dir of
                    N   ->  Just (coords (-1) 0)
                    NE  ->  Just (coords (-1) 1)
                    E   ->  Just (coords 0 1)
                    SE  ->  Just (coords 1 1)
                    S   ->  Just (coords 1 0)
                    SW  ->  Just (coords 1 (-1))
                    W   ->  Just (coords 0 (-1))
                    NW  ->  Just (coords (-1) (-1))
                    _   ->  Nothing

type Locatable a = { a | area:Area a, location:Coords }

type DirectionalSignal a = { who:Locatable a
                           , targetDir:Directions
                           }

type LocationSignal a = { who:Locatable a
                        , target:Coords
                        }

inDir:((LocationSignal a)->b)->(DirectionalSignal a)->(Maybe b)
inDir f dSig = let from = dSig.who
                   proposed = from+(dSig.targetDir)
                in
                  case proposed of
                      Nothing -> Nothing
                      Just target -> f {who=from, target=target}
