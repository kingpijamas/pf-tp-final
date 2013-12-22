module Area where
import Matrix

type Terrain = Matrix Locatable

type Locatable a = { a | whr:Terrain, coords:Position }









{-|	pun not intended
	-}
--(+) : Location -> Location -> Location
--(+) {r1,c1} {r2,c2} = {r1+r2, c1+c2}

--(-) : Location -> Location -> Location
--(-) {r1,c1} {r2,c2} = {r1-r2, c1-c2}

