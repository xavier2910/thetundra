{-# LANGUAGE ImportQualifiedPost #-}
module Story (
    start,
) where

import Control.Monad.State (
    MonadState (put),
    State,
    gets,
 )
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Engine (
    Direction (..),
    GameState,
    HasDescription (description),
    Location,
    Relation (InLoose, InStrict, OnLoose, OnStrict, VerbPhrase),
    Tree (..),
    children,
    emptyLocation,
    location,
    object,
    value,
 )
import Engine.CommandProcessor (
    CommandType (Open, Push, Steal, Turn),
 )
import Story.PCd1 (
    hangarB,
 )
import Story.Utils (
    commandOnlyObject,
    commandsShow,
    examineOnlyObject,
    examineShows,
 )

start :: Tree Direction Location
start =
    Node
        ( location
            ( "You are looking into the mouth of a dark cave in the side of a "
                ++ "tall, icy cliff to the immediate east. Surrounding you is a "
                ++ "tundra so open you can even see your hand in front of your face."
            )
            [ examineOnlyObject
                "cave"
                "The cave appears to be of the dark and rocky variety."
            , examineOnlyObject
                "cliff"
                "The cliff is covered in ice and looks very treacherous."
            , examineOnlyObject
                "tundra"
                "There is snow everywhere. Slate-grey sky."
            ]
        )
        ( M.fromList
            [ (E, incave)
            , (IN, incave)
            , (W, emptiness)
            , (N, signpost)
            , (S, forestNorthEdge)
            ]
        )

incave :: Tree Direction Location
incave =
    Node
        ( location
            ( "You are standing in a very dark cave. You can barely make out "
                ++ "a sloping upward passage to the west. In the middle of the room is "
                ++ "a round hole with a ladder in it."
            )
            [ object
                "penny"
                [OnLoose "the ground", VerbPhrase "glints"]
                "a shiny penny"
                ( M.fromList
                    [ examineShows "The penny is very shiny indeed, but it has no face."
                    ]
                )
            , object
                "chair"
                [InLoose "a dark corner of the cave", VerbPhrase "crouches"]
                "a small chair about three inches tall"
                ( M.fromList
                    [ examineShows "This chair appears to be made out of gold."
                    ]
                )
            , examineOnlyObject
                "cave"
                "The cave is definitely stone. The floor, however, is tumbled and uncomfortable under your feet. And very sharp."
            , examineOnlyObject
                "passage"
                ( "Pale light gleams at the end of the stony, rough passage. Although the cave is natural, the passage appears to be somewhat "
                    ++ "artificial in origin, although whoever hewed it was not terribly skilled at his craft."
                )
            , examineOnlyObject
                "ladder"
                ( "The ladder, though old, appears very strong. It decends down a roughly cylindrical hole into darker darkness than the darkness "
                    ++ "of the dark cave. Then again, the dark cave has some light from that passage."
                )
            ]
        )
        ( M.fromList
            [ (W, start)
            , (OUT, start)
            , (D, tjoint)
            ]
        )

tjoint :: Tree Direction Location
tjoint =
    Node
        ( location
            ( "You are hanging on to an ancient steel ladder on the wall of a small circular vertical hole. "
                ++ "The ladder stretches both up and down a fair long ways. Up you see a pale grey circle marking "
                ++ "the top of this passage. Down is only darkness. There is a door in the wall, to the north."
            )
            [ examineOnlyObject
                "ladder"
                "The ladder is made up of strong steel \'U\'s set in the smooth stone tunnel wall."
            , commandOnlyObject
                "door"
                ( M.fromList
                    [ examineShows $
                        "The door is a round affair, more like a hatch than anything. There is a warning written above "
                            ++ "the steering wheel-shaped handle."
                    , (Open, openDoor)
                    ]
                )
            , object
                "warning"
                [OnStrict "door", VerbPhrase "written, funnily enough, by hand,"]
                "a warning sign"
                ( M.fromList
                    [ examineShows "The warning reads: \n  U. S. T.\nFuel Storage\n NO SMOKING"
                    ]
                )
            , object
                "handle"
                [OnLoose "the door", VerbPhrase "lies centered"]
                "a metal circular handle"
                ( M.fromList
                    [ examineShows $
                        "Not only is it shaped like a steering wheel, it appears to be a literal "
                            ++ "steering wheel, repurposed as one of those submarine hatch screwy handles."
                    , (Turn, openDoor)
                    ]
                )
            ]
        )
        ( M.fromList
            [ (U, incave)
            , (D, teleporterBay)
            ]
        )
  where
    openDoor :: State GameState String
    openDoor = do
        put modifiedTjoint
        return "The wheel turns smoothly, as if it had been recently oiled. The door opens into a dark room."

modifiedTjoint :: Tree Direction Location
modifiedTjoint = 
    Node 
        (value tjoint)
        (M.insert IN fuelStorage . M.insert N fuelStorage
            $ fromMaybe M.empty (children tjoint)
        )


fuelStorage :: Tree Direction Location
fuelStorage =
    Node
        ( location
            "Large round tanks fill this otherwise bare room. A circular hatch leads out to the south."
            [ object
                "tanks"
                []
                ""
                ( M.fromList $
                    examineShows "The tanks are made of smooth metal and marked with various scary warnings in bold type. They look quite heavy."
                        : commandsShow [Open, Push, Steal, Turn] "That sounds like a bad idea."
                )
            ]
        )
        ( M.fromList
            [ (OUT, modifiedTjoint)
            , (S, modifiedTjoint)
            ]
        )

teleporterBay :: Tree Direction Location
teleporterBay =
    Node
        ( location
            ( "You find yourself in a large steel-walled room. A metal ladder leads up through a hole in the ceiling. "
                ++ "The only light comes from a glowing blue circle set in the opposite wall."
            )
            [ examineOnlyObject
                "circle"
                ( "The circle is very blue and glowey ring, about 6ft in diameter. There is a large, round button set in "
                    ++ "the wall in the center."
                )
            , object
                "button"
                [InStrict "circle", VerbPhrase "is set"]
                "a circular, shiny button"
                ( M.fromList
                    [ examineShows "The button is the size of your hand, and bears this sigil: \"->\"."
                    ,
                        ( Push
                        , do
                            put teleporterZap
                            gets (description . value)
                        )
                    ]
                )
            ]
        )
        ( M.fromList
            [ (U, tjoint)
            ]
        )

teleporterZap :: Tree Direction Location
teleporterZap =
    Jump
        (emptyLocation "There is a brilliant flash of blue, and you are sucked forward violently.")
        teleporterExpoDump

teleporterExpoDump :: Tree Direction Location
teleporterExpoDump =
    Jump
        ( emptyLocation $
            "You find yourself floating in nothing. You can see nothing. Out of nowhere, a voice begins to speak:\n"
                ++ "\n\t\"Welcome. Thank you for your service. You are heading into danger for the sake of all humanity. "
                ++ "An alien race, the Arcturi, are planning to invade the earth tomorrow. They have set up a staging "
                ++ "base on Proxima Centauri-d. You must infiltrate this base and stop them from launching their fleet. "
                ++ "They must be stopped at all costs short of destruction of Earth or Sun. You will land in hangar B "
                ++ "on PC-d-1, the first moon of PC-d. Good luck.\"\n"
                ++ "\nYou again are pulled violently forward. There is a flash of blue, and you find yourself in a large hangar."
        )
        hangarB

emptiness :: Tree Direction Location
emptiness =
    Node
        ( location
            "You are surrounded by nondescript, empty tundra"
            [ examineOnlyObject
                "tundra"
                "There is snow everywhere. Slate-grey sky."
            ]
        )
        ( M.fromList
            [ (E, start)
            , (NE, signpost)
            , (SE, forestNorthEdge)
            ]
        )

signpost :: Tree Direction Location
signpost =
    Node
        ( location
            ( "In the middle of the desolate tundra stands a battered old signpost. "
                ++ "You can see nothing else all the way to the horizon."
            )
            [ examineOnlyObject
                "tundra"
                "There is snow everywhere. Slate-grey sky."
            , examineOnlyObject
                "signpost"
                ( "The weathered wooden  sign points south;\n"
                    ++ "written right there  are strange symbols:\n"
                    ++ "\tU. S. T."
                )
            ]
        )
        ( M.fromList
            [ (S, start)
            , (SW, emptiness)
            ]
        )

forestNorthEdge :: Tree Direction Location
forestNorthEdge =
    Node
        ( location
            ( "You are standing at the north edge of a huge pine forest "
                ++ "stretching east and west as far as you can see. To your north is a "
                ++ "boundless tundra with a cliff stretching north on your east."
            )
            [ examineOnlyObject
                "tundra"
                ( "The tundra does indeed appear hopelessly boundless. It is just "
                    ++ "one huge swath of white."
                )
            , examineOnlyObject
                "forest"
                ( "Trees. Many trees. All conifers, and all covered in snow. Appears rather dark in "
                    ++ "there. A rather dense forest, all in all. You might even not be able to "
                    ++ "see your hand in front of your face in there."
                )
            ]
        )
        ( M.fromList
            [ (N, start)
            , (NW, emptiness)
            ]
        )