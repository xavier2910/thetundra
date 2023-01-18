module Story 
    ( start 
    ) 
  where

import Engine
    ( Location, emptyLocation, location
    , Tree (Node)
    , Direction (N, W, E, S, NE, SE, SW, NW)
    , object
    , commandOnlyObject
    , Relation (OnLoose, InLoose, VerbPhrase)
    )

import Engine.CommandProcessor 
    ( CommandType (Examine)
    )

import qualified Data.Map as M

start :: Tree Direction Location
start = Node (location  ("You are looking into the mouth of a dark cave in the side of a "
                           ++ "tall, icy cliff to the immediate east. Surrounding you is a "
                           ++ "tundra so open you can even see your hand in front of your face.")
                        [ commandOnlyObject 
                            "cave"
                            (M.fromList
                                [ (Examine, "The cave appears to be of the dark and rocky variety.")
                                ]
                            )
                        , commandOnlyObject 
                            "cliff"
                            (M.fromList
                                [ (Examine, "The cliff is covered in ice and looks very treacherous.")
                                ]
                            )
                        , commandOnlyObject 
                            "tundra"
                            (M.fromList
                                [ (Examine, "There is snow everywhere. Slate-grey sky.")
                                ]
                            )
                        , commandOnlyObject 
                            "north"
                            (M.fromList
                                [ (Examine, "Snow, snow and more snow.")
                                ]
                            )
                        , commandOnlyObject 
                            "n"
                            (M.fromList
                                [ (Examine, "Snow, snow and more snow.")
                                ]
                            )
                        , commandOnlyObject 
                            "northwest"
                            (M.fromList
                                [ (Examine, "Snow, snow and more snow.")
                                ]
                            )
                        , commandOnlyObject 
                            "nw"
                            (M.fromList
                                [ (Examine, "Snow, snow and more snow.")
                                ]
                            )
                        , commandOnlyObject 
                            "northeast"
                            (M.fromList
                                [ (Examine, "Snow, snow and more snow. Also the cliff you're next to.")
                                ]
                            )
                        , commandOnlyObject 
                            "ne"
                            (M.fromList
                                [ (Examine, "Snow, snow and more snow. Also the cliff you're next to.")
                                ]
                            )
                        , commandOnlyObject 
                            "west"
                            (M.fromList
                                [ (Examine, "Snow, snow and more snow. To the horizon. It appears flat.")
                                ]
                            )
                        , commandOnlyObject 
                            "w"
                            (M.fromList
                                [ (Examine, "Snow, snow and more snow. To the horizon. It appears flat.")
                                ]
                            )
                        , commandOnlyObject 
                            "south"
                            (M.fromList
                                [ (Examine, "Snow, snow and more snow. But perhaps a treeline.")
                                ]
                            )
                        , commandOnlyObject 
                            "s"
                            (M.fromList
                                [ (Examine, "Snow, snow and more snow. But perhaps a treeline.")
                                ]
                            )
                        , commandOnlyObject 
                            "southwest"
                            (M.fromList
                                [ (Examine, "Snow, snow and more snow.")
                                ]
                            )
                        , commandOnlyObject 
                            "sw"
                            (M.fromList
                                [ (Examine, "Snow, snow and more snow.")
                                ]
                            )
                        , commandOnlyObject 
                            "southeast"
                            (M.fromList
                                [ (Examine, "Snow, snow and more snow. Also the cliff you're next to.")
                                ]
                            )
                        , commandOnlyObject 
                            "se"
                            (M.fromList
                                [ (Examine, "Snow, snow and more snow. Also the cliff you're next to.")
                                ]
                            )
                        
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
                        ]
              )
              (M.fromList [ (W, start) ])

emptiness :: Tree Direction Location
emptiness = Node (emptyLocation "You are surrounded by nondescript, empty tundra")
                 (M.fromList [ (E, start)
                             , (NE, signpost)
                             , (SE, forestNorthEdge) ])

signpost :: Tree Direction Location
signpost = Node (emptyLocation $ "In the middle of the desolate tundra stands a battered old signpost. "
                              ++ "You can see nothing else all the way to the horizon.")
                (M.fromList [ (S, start)
                            , (SW, emptiness) ])

forestNorthEdge :: Tree Direction Location
forestNorthEdge = Node (emptyLocation $ "You are standing at the north edge of a huge pine forest "
                                     ++ "stretching east and west as far as you can see.")
                       (M.fromList [ (N, start)
                                   , (NW, emptiness) ])
                       