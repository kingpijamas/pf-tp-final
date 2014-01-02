module Area where
import Matrix as M
import Either as E
import Directions as D
import Automaton as A
import MaybeMonad as Mb

(>>=) = (>>=)  {-- FIXME Hack, works this way apparently... neither Auto.>>> nor Auto.(>>>) work --}

type Coords = M.Position
coords = M.position

getX:Coords->Int
getX coords = M.row coords

getY:Coords->Int
getY coords = M.col coords

type Area a = M.Matrix a
type Locatable a = { a | area:Area a, location:Coords }

get = M.get
add = M.add
remove = M.remove



--TODO Undoable unless it's either a Matrix of Locatables
--update:(Area a)->(Locatable a)->Maybe (Area a)
--update area elem = M.add area elem.location elem

type LocationSignal a = { who:Locatable a
                        , target:Coords
                        }

locationSignal:Locatable a -> Coords -> LocationSignal a
locationSignal who target = {who=who, target=target}

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


addCoord:Coords->Coords->M.Position
addCoord a b = coords ((getX a)+(getX b)) ((getY a)+(getY b))

addDir:Coords->D.Direction->Maybe(Coords)
addDir a dir = case (asCoords dir) of
                  Just b -> Just (addCoord a b)
                  Nothing -> Nothing

type DirectionalSignal a = { who:Locatable a
                           , targetDir:D.Direction
                           }

toLocSig : DirectionalSignal a -> Maybe (LocationSignal a)
toLocSig dSig = let 
                    who = dSig.who
                    to = (who.location) `addDir` (dSig.targetDir)
                    locationSignal' to = Mb.return (locationSignal who to)
                 in
                    to >>= locationSignal'