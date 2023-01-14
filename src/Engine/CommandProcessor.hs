module Engine.CommandProcessor
    ( stringToCommand
    , executeCommand
    , readM
    ) 
  where


import Engine
    ( GameState
    , Direction (..)
    , children
    , HasDescription (description)
    , value
    )

import Control.Monad.Except
    ( throwError
    , catchError
    )

import Control.Monad.State
    ( State
    , get
    , gets
    , put
    )    

import qualified Data.Map as M

import Data.Char 
    ( toLower
    , toUpper
    )


data Command 
    = MoveCommand CommandType Args
    | ObjectCommand CommandType Args
    | PlayerCommand CommandType Args
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
    , (Inventory, PlayerCommand Inventory)
    , (Wait, OtherCommand Wait)
    , (Again, OtherCommand Again)
    , (Help, OtherCommand Help)
    ]

directionLonghandMap :: M.Map String Direction
directionLonghandMap = M.fromList
    [ ("north", N)
    , ("south", S)
    , ("east", E)
    , ("west", W)
    , ("northwest", NW)
    , ("northeast", NE)
    , ("southwest", SW)
    , ("southeast", SE)
    ]

readM :: (Read r) => String -> Maybe r
readM str = case reads str of 
    [(x, _)] -> return x
    _ -> throwError ()

-- | helper function to account for longhand
-- its a thinking crutch for me
parseDirection :: String -> Maybe Direction
parseDirection str = do
    let up = map toUpper str
    let down = map toLower str
    readM up 
        `catchError` (\_ -> M.lookup down directionLonghandMap)

stringToCommand :: String -> Maybe Command
-- | supports commands directly corresponding to `Command`
-- syntax + shorthand. todo: support just 'north'/'n' etc
stringToCommand s = do
    -- if only there was a warning disabler... this is not a bug
    let cmd:args = case words $ map toLower s of 
            (c:as) -> c:as
            [] -> [""]
    commandType <- readM (toUpper (head cmd) : tail cmd) -- n. b. we must capitalize the first letter for reads' sake
        `catchError` (\_ -> M.lookup cmd commandShorthandMap)
    command <- M.lookup commandType commandMap
    return $ command args

move :: Direction -> GameState -> Maybe GameState
move dir st = do
    chs <- children st
    M.lookup dir chs

-- | a stateful computation that takes a command and 
-- \"changes\" the current location, eventually inventory,
-- etc. and returns a message to display to the user
executeCommand :: Command -> State GameState String

executeCommand (MoveCommand Go args) = do
    here <- get
    let destination = destinationFrom here
    case destination of
        Just d -> do
            put d
            return $ (description . value) d
        Nothing -> 
            return $ head args ++ " is not a direction you can go :("
  where
    destinationFrom place = do
        dir <- parseDirection $ head args
        move dir place

executeCommand (MoveCommand cmd _) = 
    return $ "<error: " ++ show cmd ++ " is not a movement command. this is a bug>"

executeCommand (ObjectCommand typ args) = return "" -- todo
executeCommand (PlayerCommand _ _) = return "unfortunately, player commands arent supported yet :("
executeCommand (OtherCommand Look _) = gets (description . value)
executeCommand (OtherCommand Wait _) = return "You do nothing in anticipation of what might happen...."
executeCommand (OtherCommand Help _) = return $
    "In The Tundra, unlike The Cave or The Forest, one does not select options, but "
    ++ "rather types in commands, such as \"go north\" and the like. Here is a list "
    ++ "of common commands and a short description of each.\n\n\n"
    ++ "go [direction]\n\tGo in the indicated cardinal direction. you will be "
    ++ "informed of what things lie in various directions.\n\n"
    ++ "look (shortcut 'l')\n\tDisplay the surrounding environment. helpful if theres a bunch of "
    ++ "clutter in your terminal.\n\n"
    ++ "again (shortcut 'g')\n\tDo whatever you just did, again.\n\n"
    ++ "inventory (shortcut 'i')\n\take inventory. Currently not implemented :(\n\n"
    ++ "examine [thing] (shortcut 'x')\n\tTake a more detailed look at [thing]. "
    ++ "Ideally, [thing] should be the one-word name of an object in your immediate "
    ++ "environment. Hopefully i can get some nice support for this up; this is an "
    ++ "essential command. \"When in doubt, examine more.\" This is perhaps the one "
    ++ "most important command. Use it liberally.\n\n"
    ++ "help (shortcut '?')\n\tDisplay this help text. I have no ambitions for contextual "
    ++ "help or a walkthru tutorial. If you want it, make it. This is open source, after all.\n\n"
    ++ "wait (shortcut 'z')\n\tWait. Pretty simple.\n\n"
    ++ "Have fun!"
executeCommand (OtherCommand cmd _) = 
    return $ "<error: " ++ show cmd ++ " is not an \"other\" command. this is a bug>"
