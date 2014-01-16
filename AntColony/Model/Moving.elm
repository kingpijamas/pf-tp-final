module AntColony.Model.Moving where
-- TODO this should go in Model, not in Model.Ant
import AntColony.Capacities.Moving as Mv
import open AntColony.Model.Terrain
import open AntColony.Utils.MaybeMonad

type Obstacle = Occupant

type OccupiationF = Mv.OccupiationF Tile
type EvictionF = Mv.EvictionF Tile

occupy : OccupiationF                            -- : Terrain -> Coords -> a -> Maybe (Terrain)
occupy terrain pos occ = let occupyTile tile = case tile.occupant of
                                                    Nothing -> return { tile | occupant <- occ }
                                                    _ -> Nothing

                             updateTerrain tile' = add terrain pos tile'
                          in
                            (terrain `get` pos)     -- : Maybe (Tile)
                             >>= (occupyTile)       -- : Tile -> Maybe(Tile)
                             >>= (updateTerrain)    -- : Tile -> Maybe(Terrain)

evict : EvictionF
evict terrain pos = let evictTile tile = case tile.occupant of
                                            Just occ -> return { tile | occupant <- occ }
                                            _   ->  Nothing

                        updateTerrain tile' = return (remove terrain pos tile', tile')
                        
                        --TODO smells like the 'State' monad could probably help here
                        --it's either that or some weird stuff with 'join' or 'maybe'
                        flatten (mbarea',tile') = case (mbarea') of
                                                    Just area' -> return (area', tile')
                                                    _   -> Nothing
                     in
                       (terrain `get` pos)         -- : Maybe(Tile)
                        >>= (evictTile)            -- : Tile -> Maybe(Tile)
                        >>= (updateTerrain)        -- : Tile -> Maybe(Maybe(Terrain), Tile)
                        >>= (flatten)              -- : (Maybe(Terrain),Tile) -> Maybe(Terrain, Tile)

type Motor = Mv.Motor Tile -- : Automaton (DirectionSignal) (Maybe(Terrain))

motor : Terrain -> Motor
motor area = Mv.motor occupy evict area