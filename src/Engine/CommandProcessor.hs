module Engine.CommandProcessor
    ( Command (..)
    , CommandType (..) 
    , stringToCommand
    , readM
    ) 
  where


import qualified Data.Map as M

import Control.Monad.Except
    ( throwError
    , catchError
    )

import Data.Char 
    ( toLower
    , toUpper
    )


data Command 
    = MoveCommand CommandType Args
    | ObjectCommand CommandType Args
    | OtherCommand CommandType Args
  deriving (Show)

type Args = [String]

data CommandType 
    = Go
    | Examine
    | Look
    | Inventory
    | Wait
    | Again
    | Help 
  deriving (Eq, Ord, Bounded, Show, Read)


-- | this is only necessary for shortcuts as
-- `CommandType`'s derived `Read` instance
-- can do the heavy lifting for us
commandShorthandMap :: M.Map String CommandType
commandShorthandMap = M.fromList
    [ ("x", Examine)
    , ("l", Look)
    , ("i", Inventory)
    , ("z", Wait)
    , ("g", Again)
    , ("?", Help)
    ]

commandMap :: M.Map CommandType (Args -> Command)
commandMap = M.fromList
    [ (Go, MoveCommand Go)
    , (Examine, ObjectCommand Examine)
    , (Look, OtherCommand Look)
    , (Inventory, OtherCommand Inventory)
    , (Wait, OtherCommand Wait)
    , (Again, OtherCommand Again)
    , (Help, OtherCommand Help)
    ]

readM :: (Read r) => String -> Maybe r
readM str = case reads str of 
    [(x, _)] -> return x
    _ -> throwError ()

stringToCommand :: String -> Maybe Command
stringToCommand [] = throwError ()
stringToCommand s = do
    let cmd:args = words $ map toLower s
    commandType <- readM (toUpper (head cmd) : tail cmd) -- n. b. we must capitalize the first letter for reads' sake
        `catchError` (\_ -> M.lookup cmd commandShorthandMap)
    command <- M.lookup commandType commandMap
    return $ command args
