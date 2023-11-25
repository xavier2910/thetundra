module Story.PCd1 (
    hangarB,
) where

import qualified Data.Map as M
import Engine (Direction, Location, Tree (..), location)

hangarB :: Tree Direction Location
hangarB =
    Node
        ( location
            ( "You are standing at the east end of a huge hangar, but it is pretty dark, "
                ++ "although warm. There are many heat lamps on the ceiling. Spaceships are lined "
                ++ "up in rows as far as you can see."
            )
            []
        )
        M.empty
