module Model.Ant.Loading where
-- TODO this should go in Model, not in Model.Ant
import open Utils.Tuple
import open Utils.MaybeMonad
import open Model.Terrain
import Capacities.Loading as Ld
import Model.Food as Fd

type LoadF = Ld.LoadF Tile FoodT

--load : (FoodCarrier a) -> Food -> Maybe (FoodCarrier a, Maybe(Food))
load : LoadF    -- : Terrain -> Coords -> Food -> Maybe(Terrain, Maybe(Food))
load terrain ldrPos fd = let asAnt x = Ant x
                             asNest x = AntNest x

                             load' occ = case occ of
                                            Ant ant -> asAnt `mapFst` (Fd.load ant fd)
                                            AntNest nest -> asNest `mapFst` (Fd.load nest fd)
                                            _ -> Nothing

                             updateTerrain tile' = terrain `add` tile'

                          in (terrain `get` ldrPos)         -- : Maybe(Tile)
                              >>= (.occupant)               -- : Tile -> Maybe(Occupant)
                              >>= (load')                   -- : Occupant -> Maybe(Tile, Maybe(Food))
                              >>= (mapFst updateTerrain)    -- : (Tile, Maybe(Food)) -> Maybe(Maybe(Terrain),Maybe(Food))
                              >>= (joinFst)                 -- : (Maybe(Terrain),Maybe(Food)) -> Maybe(Terrain, Maybe(Food))

--type LoadF a c = (Area a) -> Coords -> c -> Maybe(Area a)

--type UnloadF a c = (Area a) -> Coords -> Maybe(Area a, c)


type Loader a = Automaton (LoadSignal) (Maybe(Area a))

loader : (LoadF a c) -> (UnloadF a c) -> (Area a) -> (Loader a)
loader ld unld area = pure (loadProxy ld unld area)