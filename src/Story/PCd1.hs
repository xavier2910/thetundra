module Story.PCd1 (
    hangarB,
) where

import Control.Monad.State (
    MonadState (put),
    gets,
 )
import qualified Data.Map as M
import Engine (
    Direction (..),
    HasDescription (description),
    Location,
    Relation (VerbPhrase),
    Tree (..),
    emptyLocation,
    location,
    object,
    value,
 )
import Engine.CommandProcessor (
    CommandType (Steal),
 )
import Story.Utils (
    examineShows,
 )

hangarB :: Tree Direction Location
hangarB =
    Node
        ( location
            ( "You are standing at the east end of a huge hangar, but it is pretty dark, "
                ++ "although warm. There are many heat lamps on the ceiling. Spaceships are lined "
                ++ "up in rows as far as you can see."
            )
            [ object
                "spaceship"
                [ VerbPhrase "towers over you"
                ]
                "The nearest spaceship's gleaming hull"
                ( M.fromList
                    [ examineShows "The large spaceship is bright red. A ramp leads up to it."
                    ,
                        ( Steal
                        , do
                            put intoShip
                            gets $ description . value
                        )
                    ]
                )
            ]
        )
        (M.fromList
            [
                (IN, intoShip)
            ]
        )

intoShip :: Tree Direction Location
intoShip = Jump (emptyLocation "You climb up into the spaceship. Despite the fact that it's strangely and unintuitively laid out, you make it to the cockpit.") insideShip

insideShip :: Tree Direction Location
insideShip =
    Node
        ( location
            ("")
            []
        )
        M.empty
