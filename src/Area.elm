module Area where
import src/Matrix.elm as Matrix

type Area a = Matrix.Matrix a

type Coords = Matrix.Position

coords = position

getX:Coords->Int
getX loc = loc.row

getY:Coords->Int
getY loc = loc.col

type Location = { area:Area
				, coords:Coords
				}

type Locatable a = { a | location:Location }

{--TODO move--}
{--TODO add--}
{--TODO remove?--}
