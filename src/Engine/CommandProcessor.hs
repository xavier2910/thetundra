module Engine.CommandProcessor
    ( stringToCommand
    , executeCommand
    , readM
    , CommandType (..)
    )
  where


import Engine
    ( GameState
    , Direction (..)
    , children
    , HasDescription (description)
    , value, Location (objects), name, Object (commands), Place, treeGet, tzHere, tzGoto, getid
    )

import Control.Applicative
    ( Alternative ( (<|>) )
    )
import Control.Monad.Except
    ( throwError
    , guard
    )

import qualified Data.Map as M

import Data.Char
    ( toLower
    , toUpper
    )

import Data.Maybe 
    ( fromMaybe
    )

import Control.Monad.State
    ( gets, State
    )


data Command = Command CommandType Args
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
  deriving (Eq, Ord, Bounded, Enum, Show, Read)


objectCommands :: [CommandType]
objectCommands =
    [ Examine
    ]

moveCommands :: [CommandType]
moveCommands =
    [ Go
    ]

playerCommands :: [CommandType]
playerCommands =
    [ Inventory
    ]

-- this is only necessary for shortcuts as
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
    , ("down", D)
    , ("up", U)
    ]

readM :: (Read r) => String -> Maybe r
readM str = case reads str of
    [(x, _)] -> return x
    _ -> throwError ()

-- | helper function to account for longhand
-- it's a thinking crutch for me
parseDirection :: String -> Maybe Direction
parseDirection str = do
    let up = map toUpper str
    let down = map toLower str
    readM up -- the directions are constructors in all uppercase
        <|> M.lookup down directionLonghandMap -- the longhands are in a map and lowercase by convention


-- | supports commands directly corresponding to `Command`
-- syntax + shorthand and bare directions. 
stringToCommand :: String -> Maybe Command
stringToCommand s = do

    guard $ (not . null) cmdAndArgs
    guard $ (not . null) cmd

    parsed
        <|> parsedShorthand
        <|> parsedDirectionShorthand

  where
    cmdAndArgs = words $ map toLower s
    cmd = head cmdAndArgs
    args = tail cmdAndArgs

    -- n. b. we must capitalize the first letter for reads' sake
    parsed = do
        cmdType <- readM (toUpper (head cmd) : tail cmd) 
        return $ Command cmdType args

    parsedShorthand = do
        cmdType <- M.lookup cmd commandShorthandMap
        return $ Command cmdType args

    parsedDirectionShorthand = do
        -- check to make sure cmd is a direction:
        -- it'll get parsed later on
        _ <- parseDirection cmd
        return $ Command Go (cmd : args)


move :: Direction -> Place -> State GameState Place
move dir st = do
    targetPlace <- treeGet targetId
    return $ fromMaybe st targetPlace

  where
    targetId = fromMaybe
      "" -- so what this value is doesn't matter, provided it's not a valid TreeID...
      (do chs <- children st
          M.lookup dir chs)

-- | a stateful computation that takes a command and 
-- \"changes\" the current location, eventually inventory,
-- etc. and returns a message to display to the user
executeCommand :: Command -> State GameState String

executeCommand (Command Go args) = do
    if (not . null) args
        then do
            here <- tzHere
            let destination = destinationFrom here
            case destination of
                Just d -> do
                    dest <- d
                    tzGoto $ getid dest
                    return $ (description . value) dest
                Nothing ->
                    return $ head args ++ " is not a direction you can go :(" 

        else return "In what direction?"
  where
    destinationFrom place = do
        dir <- parseDirection $ head args
        chs <- children place
        guard $ dir `elem` M.keys chs -- (move does not fail on bad direction)
        return $ move dir place


executeCommand (Command Look _) = gets (description . value . snd)
executeCommand (Command Wait _) = return "You do nothing in anticipation of what might happen...."
executeCommand (Command Help _) = return $
    "In The Tundra, unlike The Cave or The Forest, one does not select options, but "
    ++ "rather types in commands, such as \"go north\" and the like. Here is a list "
    ++ "of common commands and a short description of each.\n\n\n"
    ++ "go [direction]\n\tGo in the indicated cardinal direction. You will be "
    ++ "informed of what things lie in various directions.\n\n"
    ++ "look (shortcut 'l')\n\tDisplay the surrounding environment. Helpful if there's a bunch of "
    ++ "clutter in your terminal.\n\n"
    ++ "again (shortcut 'g')\n\tDo whatever you just did, again.\n\n"
    ++ "inventory (shortcut 'i')\n\tTake inventory. Currently not implemented :(\n\n"
    ++ "examine [thing] (shortcut 'x')\n\tTake a more detailed look at [thing]. "
    ++ "Ideally, [thing] should be the one-word name of an object in your immediate "
    ++ "environment. Hopefully I can get some nice support for this up; this is an "
    ++ "essential command. \"When in doubt, examine more.\" This is perhaps the one "
    ++ "most important command. Use it liberally.\n\n"
    ++ "help (shortcut '?')\n\tDisplay this help text. I have no ambitions for contextual "
    ++ "help or a walkthru tutorial. If you want it, make it. This is open source, after all.\n\n"
    ++ "wait (shortcut 'z')\n\tWait. Pretty simple.\n\n"
    ++ "These are not the only commands, feel free to try what makes sense in context.\n\n"
    ++ "Have fun!"


executeCommand (Command cmd args)
    | cmd `elem` objectCommands = do
        if (not . null) args
            then do
                here <- tzHere
                let loc = value here
                    objs = objects loc
                    found = [obj | obj <- objs, name obj == head args]
                if (not . null) found then
                    (do
                        let xDescription
                                = fromMaybe "" $ M.lookup cmd (commands $ head found)    
                        return xDescription
                    )
                    else return $ "What " ++ head args ++ "?"

            else return "what object?"


executeCommand (Command cmd _) =
    return $ "<error: " ++ show cmd ++ " is not a supported command. this is a bug>"
