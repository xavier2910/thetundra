module Story.Utils (
    examineOnlyObject,
    examineShows,
) where

import Control.Monad.State (State)
import qualified Data.Map as M
import Engine (GameState, Object, object)
import Engine.CommandProcessor (CommandType (Examine))

examineOnlyObject :: String -> String -> Object
examineOnlyObject n xdesc = object n [] "" (M.fromList [examineShows xdesc])

examineShows :: String -> (CommandType, State GameState String)
examineShows msg = (Examine, return msg)
