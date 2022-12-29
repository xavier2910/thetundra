module Story (start) where

import Engine
    ( Location, emptyLocation, location
    , Tree(Node)
    , Direction(N, W, E, S)
    , object )
import qualified Data.Map as M

start :: Tree Direction Location
start = Node (emptyLocation $ "You are looking into the mouth of a dark cave in the side of a\n"
                           ++ "tall, icy cliff to the immediate east. Surrounding you is a\n"
                           ++ "tundra so open you can even see your hand in front of your face.")
             (M.fromList [ (E, incave)
                         , (W, emptiness)
                         , (N, signpost)
                         , (S, forestNorthEdge) ])

incave :: Tree Direction Location
incave = Node (location ("You are standing in a very dark cave. You can barely make out\n"
                      ++ "a sloping upward passage to the west.")
                        [ object "A shiny penny sits on the floor."
                        , object "Nearby is a small chair, about three inches tall." ])
              (M.fromList [ (W, start) ])

emptiness :: Tree Direction Location
emptiness = Node (emptyLocation "You are surrounded by nondescript, empty tundra")
                 (M.fromList [ (E, start) ])

signpost :: Tree Direction Location
signpost = Node (emptyLocation $ "In the middle of the desolate tundra stands a battered old signpost.\n"
                              ++ "You can see nothing else all the way to the horizon.")
                (M.fromList [ (S, start) ])

forestNorthEdge :: Tree Direction Location
forestNorthEdge = Node (emptyLocation $ "You are standing at the north edge of a huge pine forest\n"
                                     ++ "stretching east and west as far as you can see.")
                       (M.fromList [ (N, start) ])