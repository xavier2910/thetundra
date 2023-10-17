module Story
    ( start
    )
  where

import Engine
    ( Location, location
    , Tree, node
    , Direction (..)
    , Object
    , object
    , Relation (OnLoose, InLoose, VerbPhrase, OnStrict, InStrict)
    , GameState
    , TreeZipper
    , treeAdd
    , initialize
    )

import Engine.CommandProcessor
    ( CommandType (Examine)
    )

import Control.Monad
    ( forM_
    )

import Control.Monad.State
    ( execState
    , State
    )

import qualified Data.Map as M



examineOnlyObject :: String -> String -> Object
examineOnlyObject n xdesc = object n [] "" (M.fromList [(Examine, xdesc)])




start :: GameState
start = execState boot zipper

zipper :: TreeZipper Direction Location
zipper = initialize playerStart

boot :: State GameState ()
boot = forM_ [playerStart, incave, emptiness, signpost, forestNorthEdge] treeAdd



playerStart :: Tree Direction Location
playerStart = node (location  ("You are looking into the mouth of a dark cave in the side of a "
                           ++ "tall, icy cliff to the immediate east. Surrounding you is a "
                           ++ "tundra so open you can even see your hand in front of your face.")

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
             "start"
             (M.fromList [ (E, incave)
                         , (W, emptiness)
                         , (N, signpost)
                         , (S, forestNorthEdge) ])

incave :: Tree Direction Location
incave = node (location ("You are standing in a very dark cave. You can barely make out "
                      ++ "a sloping upward passage to the west. In the middle of the room is "
                      ++ "a round hole with a ladder in it.")

                        [ object 
                            "penny" 
                            [OnLoose "the ground", VerbPhrase "glints"]
                            "a shiny penny"
                            (M.fromList 
                                [ (Examine, "The penny is very shiny indeed, but it has no face.")
                                ]
                            )

                        , object 
                            "chair" 
                            [InLoose "a dark corner of the cave", VerbPhrase "crouches"] 
                            "a small chair about three inches tall" 
                            (M.fromList
                                [ (Examine, "This chair appears to be made out of gold.")
                                ]
                            )

                        , examineOnlyObject
                            "cave"
                            "The cave is definitely stone. The floor, however, is tumbled and uncomfortable under your feet. And very sharp."

                        , examineOnlyObject
                            "passage"
                            ("Pale light gleams at the end of the stony, rough passage. Although the cave is natural, the passage appears to be somewhat "
                                    ++ "artificial in origin, although whoever hewed it was not terribly skilled at his craft."
                            )

                        , examineOnlyObject
                            "ladder"
                            ("The ladder, though old, appears very strong. It decends down a roughly cylindrical hole into darker darkness than the darkness "
                                    ++ "of the dark cave. Then again, the dark cave has some light from that passage."
                            )
                        
                        ]
              )
              "incave"
              (M.fromList 
                    [ (W, playerStart) 
                    , (D, tjoint)
                    ]
              )

tjoint :: Tree Direction Location
tjoint = node 
    (location
        (  "You are hanging on to an ancient steel ladder on the wall of a small circular vertical hole. "
        ++ "The ladder stretches both up and down a fair long ways. Up you see a pale grey circle marking "
        ++ "the top of this passage. Down is only darkness. There is a door in the wall, to the north."
        )
        [ examineOnlyObject
            "ladder"
            "The ladder is made up of strong steel \'U\'s set in the smooth stone tunnel wall."

        , examineOnlyObject
            "door"
            (  "The door is a round affair, more like a hatch than anything. There is a warning written above "
            ++ "the steering wheel-shaped handle."
            )

        , object
            "warning"
            [OnStrict "door", VerbPhrase "written, funnily enough, by hand,"]
            "a warning sign"
            (M.fromList
                [ (Examine, "The warning reads: \n  U. S. T.\nFuel Storage\n NO SMOKING")
                ]
            )

        , object
            "handle"
            [OnLoose "the door", VerbPhrase "lies centered"]
            "a metal circular handle"
            (M.fromList
                [   ( Examine
                    ,  "Not only is it shaped like a steering wheel, it appears to be a literal "
                        ++ "steering wheel, repurposed as one of those submarine hatch screwy handles."
                    )
                ]
            )
        ]
    )
    "tjoint"
    (M.fromList
        [ (U, incave)
        , (D, teleporterBay)
        ]
    )

teleporterBay :: Tree Direction Location
teleporterBay = node 
    (location 
        ("You find yourself in a large steel-walled room. A metal ladder leads up through a hole in the ceiling. "
            ++ "The only light comes from a glowing blue circle set in the opposite wall."
        )
        [ examineOnlyObject 
            "circle" 
            ("The circle is very blue and glowey ring, about 6ft in diameter. There is a large, round button set in "
                ++ "the wall in the center."
            )
        
        , object 
            "button"
            [InStrict "circle", VerbPhrase "is set"]
            "a circular, shiny button"
            (M.fromList
                [ (Examine, "The button is the size of your hand, and bears this sigil: \"->\".")]
            )
        ]
    ) 
    "teleporterBay"
    (M.fromList 
        [ (U, tjoint)
        ]
    )

emptiness :: Tree Direction Location
emptiness = node (location
                    "You are surrounded by nondescript, empty tundra"

                    [ examineOnlyObject
                            "tundra"
                            "There is snow everywhere. Slate-grey sky."
                    ]
                )
                "empty"
                 (M.fromList [ (E, playerStart)
                             , (NE, signpost)
                             , (SE, forestNorthEdge) ])

signpost :: Tree Direction Location
signpost = node (location
                    ("In the middle of the desolate tundra stands a battered old signpost. "
                        ++ "You can see nothing else all the way to the horizon."
                    )

                    [ examineOnlyObject
                        "tundra"
                        "There is snow everywhere. Slate-grey sky."

                    , examineOnlyObject
                        "signpost"
                        ("The weathered wooden  sign points south;\n"
                            ++ "written right there  are strange symbols:\n"
                            ++ "\tU. S. T."
                        )
                    ]
                )
                "signpost"
                (M.fromList [ (S, playerStart)
                            , (SW, emptiness) ])

forestNorthEdge :: Tree Direction Location
forestNorthEdge = node (location
                            ("You are standing at the north edge of a huge pine forest "
                                ++ "stretching east and west as far as you can see. To your north is a "
                                ++ "boundless tundra with a cliff stretching north on your east."
                            )
                            [ examineOnlyObject
                                "tundra"
                                ("The tundra does indeed appear hopelessly boundless. It is just "
                                    ++ "one huge swath of white."
                                )

                            , examineOnlyObject
                                "forest"
                                ("Trees. Many trees. All conifers, and all covered in snow. Appears rather dark in "
                                    ++ "there. A rather dense forest, all in all. You might even not be able to "
                                    ++ "see your hand in front of your face in there."
                                )
                            ]
                       )
                       "fne"
                       (M.fromList [ (N, playerStart)
                                   , (NW, emptiness) ])
