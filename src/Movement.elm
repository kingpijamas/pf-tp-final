module Movement where
import src/Actor.elm
import src/Area.elm
import src/CardinalDirs.elm

type Moving a = Actor a Maybe(Area)
{--thus, do:self->Maybe(Area)--}


{--un motor es un automata. La funcion con la que se crea el motor es la estrategia/el tipo de movimiento--}
{--Ahora bien, viendolo mejor, por no pensar el motor, solo actuar de forma reactiva, 
--seria SOLO el TIPO de movimiento la funcion, y NO la estrategia--}




type Motor a = Actor (Locatable a) Maybe(Area)

asCoords:CardinalDirs->Maybe(Coords)
asCoords dir = case dir of
                    North   ->  Just coords (-1) 0
                    East    ->  Just coords 0 1
                    South   ->  Just coords 1 0
                    West    ->  Just coords 0 (-1)
                    _       ->  Nothing
