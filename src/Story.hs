module Story 
    ( start 
    ) 
  where

import Engine
    ( Location, location
    , Tree (Node)
    , Direction (N, W, E, S, NE, SE, SW, NW)
    , Object
    , object
    , Relation (OnLoose, InLoose, VerbPhrase)
    )

import Engine.CommandProcessor 
    ( CommandType (Examine)
    )

import qualified Data.Map as M



examineOnlyObject :: String -> String -> Object
examineOnlyObject n xdesc = object n [] "" (M.fromList [(Examine, xdesc)])



start :: Tree Direction Location
start = Node (location  ("You are looking into the mouth of a dark cave in the side of a "
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
             (M.fromList [ (E, incave)
                         , (W, emptiness)
                         , (N, signpost)
                         , (S, forestNorthEdge) ])

incave :: Tree Direction Location
incave = Node (location ("You are standing in a very dark cave. You can barely make out "
                      ++ "a sloping upward passage to the west.")

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
                        
                        ]
              )
              (M.fromList [ (W, start) ])

emptiness :: Tree Direction Location
emptiness = Node (location 
                    "You are surrounded by nondescript, empty tundra"

                    [ examineOnlyObject 
                            "tundra"
                            "There is snow everywhere. Slate-grey sky."
                    ]
                )
                 (M.fromList [ (E, start)
                             , (NE, signpost)
                             , (SE, forestNorthEdge) ])

signpost :: Tree Direction Location
signpost = Node (location 
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
                (M.fromList [ (S, start)
                            , (SW, emptiness) ])

forestNorthEdge :: Tree Direction Location
forestNorthEdge = Node (location 
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
                       (M.fromList [ (N, start)
                                   , (NW, emptiness) ])
                       