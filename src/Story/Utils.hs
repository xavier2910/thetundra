{-# LANGUAGE ImportQualifiedPost #-}
module Story.Utils (
    examineOnlyObject,
    commandOnlyObject,
    examineShows,
    commandShows,
    commandsShow
) where

import Control.Monad.State (
    State,
 )
import Data.Map qualified as M
import Engine (
    GameState,
    Object,
    object,
 )
import Engine.CommandProcessor (
    CommandType (Examine),
 )

examineOnlyObject :: String -> String -> Object
examineOnlyObject n xdesc = commandOnlyObject n (M.fromList [examineShows xdesc])

{- | useful to allow examination etc.
 of static scenery that should not
 be dynamically described.
-}
commandOnlyObject :: String -> M.Map CommandType (State GameState String) -> Object
commandOnlyObject s = object s [] ""

examineShows :: String -> (CommandType, State GameState String)
examineShows = commandShows Examine

commandShows :: CommandType -> String -> (CommandType, State GameState String)
commandShows cmd msg = (cmd, return msg)

commandsShow :: [CommandType] -> String -> [(CommandType, State GameState String)]
commandsShow (cmd:cmds) msg = (cmd, return msg): commandsShow cmds msg
commandsShow [] _ = []
