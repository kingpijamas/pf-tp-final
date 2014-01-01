module Area where
import Matrix as M
import Either as E
import Directions as D

type Coords = M.Position
coords = M.position

--getX:{a | row:Int}->Int
getX:Coords->Int
getX coords = M.row coords

--getY:{a | col:Int}->Int
getY:Coords->Int
getY coords = M.col coords


type Area a = M.Matrix a
type Locatable a = { a | area:Area a, location:Coords }

--TODO Undoable unless it's either a Matrix of Locatables
--update:(Area a)->(Locatable a)->Maybe (Area a)
--update area elem = M.add area elem.location elem

type LocationSignal a = { who:Locatable a
                        , target:Coords
                        }

asCoords:D.Direction->Maybe(Coords)
asCoords dir = case dir of
                    D.N   ->  Just (coords (-1) 0)
                    D.NE  ->  Just (coords (-1) 1)
                    D.E   ->  Just (coords 0 1)
                    D.SE  ->  Just (coords 1 1)
                    D.S   ->  Just (coords 1 0)
                    D.SW  ->  Just (coords 1 (-1))
                    D.W   ->  Just (coords 0 (-1))
                    D.NW  ->  Just (coords (-1) (-1))
                    _   ->  Nothing



add:Coords->Coords->M.Position
add a b = coords ((getX a)+(getX b)) ((getY a)+(getY b))

addDir:Coords->D.Direction->Maybe(Coords)
addDir a dir = case (asCoords dir) of
                  Just b -> Just (add a b)
                  Nothing -> Nothing

type DirectionalSignal a = { who:Locatable a
                           , targetDir:D.Direction
                           }

inDir:((LocationSignal a)->b)->(DirectionalSignal a)->(Maybe b)
inDir f dSig = let from = dSig.who.location
                   proposed = from `addDir` (dSig.targetDir)
                in
                  case proposed of
                      Nothing -> Nothing
                      Just to -> Just (f {who=dSig.who, target=to})
