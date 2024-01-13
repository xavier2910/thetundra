module Engine.CommandProcessor (
    stringToCommand,
    executeCommand,
    readM,
    CommandType (..),
) where

import Control.Applicative (
    Alternative ((<|>)),
 )
import Control.Monad.Except (
    guard,
    throwError,
 )
import Control.Monad.State (
    State,
    get,
    gets,
    put,
 )
import Data.Char (
    toLower,
    toUpper,
 )
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Engine (
    Direction (..),
    GameState,
    HasDescription (description),
    Location (objects),
    Object (commands),
    children,
    name,
    value,
 )

data Command = Command CommandType Args
    deriving (Show)

type Args = [String]

data CommandType
    = Go
    | Examine
    | Push
    | Look
    | Inventory
    | Wait
    | Again
    | Help
    | Steal
    | Open
    | Turn
    deriving (Eq, Ord, Bounded, Enum, Show, Read)

objectCommands :: [CommandType]
objectCommands =
    [ Examine
    , Push
    , Steal
    , Open
    , Turn
    ]

moveCommands :: [CommandType]
moveCommands =
    [ Go
    ]

playerCommands :: [CommandType]
playerCommands =
    [ Inventory
    ]

{- | this is only necessary for shortcuts as
 `CommandType`'s derived `Read` instance
 can do the heavy lifting for us
-}
commandShorthandMap :: M.Map String CommandType
commandShorthandMap =
    M.fromList
        [ ("x", Examine)
        , ("l", Look)
        , ("i", Inventory)
        , ("z", Wait)
        , ("g", Again)
        , ("?", Help)
        ]

commandAliasMap :: M.Map String CommandType
commandAliasMap =
    M.fromList
        [ ("press", Push) -- it's this way and not vice versa b/c press has fewer meanings than push. It doesn't matter much
        ]

directionLonghandMap :: M.Map String Direction
directionLonghandMap =
    M.fromList
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
        , ("into", IN)
        , ("enter", IN)
        , ("exit", OUT)
        ]

particles :: [String]
particles =
    [ "the"
    , "a"
    , "an"
    , "of"
    ]

readM :: (Read r) => String -> Maybe r
readM str = case reads str of
    [(x, _)] -> return x
    _ -> throwError ()

{- | helper function to account for longhand
 it's a thinking crutch for me
-}
parseDirection :: String -> Maybe Direction
parseDirection str = do
    let up = map toUpper str
        down = map toLower str

    readM up -- the directions are constructors in all uppercase
        <|> M.lookup down directionLonghandMap -- the longhands are in a map and lowercase by convention

{- | supports commands directly corresponding to `Command`
 syntax + shorthand and bare directions.
-}
stringToCommand :: String -> Maybe Command
stringToCommand s = do
    guard $ (not . null) cmdAndArgs

    parsed
        <|> parsedShorthand
        <|> parsedAlias
        <|> parsedDirectionShorthand
  where
    cmdAndArgs = filter (not . (`elem` particles)) $ words $ map toLower s
    cmd = head cmdAndArgs
    args = tail cmdAndArgs

    -- n. b. we must capitalize the first letter for reads' sake
    parsed = do
        cmdType <- readM (toUpper (head cmd) : tail cmd)
        return $ Command cmdType args

    parsedAlias = do
        cmdType <- M.lookup cmd commandAliasMap
        return $ Command cmdType args

    parsedShorthand = do
        cmdType <- M.lookup cmd commandShorthandMap
        return $ Command cmdType args

    parsedDirectionShorthand = do
        -- check to make sure cmd is a direction:
        -- it'll get parsed later on
        _ <- parseDirection cmd
        return $ Command Go (cmd : args)

move :: Direction -> GameState -> Maybe GameState
move dir st = do
    chs <- children st
    M.lookup dir chs

{- | a stateful computation that takes a command and
 \"changes\" the current location, eventually inventory,
 etc. and returns a message to display to the user
-}
executeCommand :: Command -> State GameState String
executeCommand (Command Go args) = do
    if (not . null) args
        then do
            here <- get
            let destination = destinationFrom here
            case destination of
                Just d -> do
                    put d
                    return $ (description . value) d
                Nothing ->
                    return $ head args ++ " is not a direction you can go :("
        else return "In what direction?"
  where
    destinationFrom place = do
        dir <- parseDirection $ head args
        move dir place
executeCommand (Command Look _) = gets (description . value)
executeCommand (Command Wait _) = return "You do nothing in anticipation of what might happen...."
executeCommand (Command Help _) =
    return $
        "In The Tundra, unlike The Cave or The Forest, one does not select options, but "
            ++ "rather types in commands, such as \"go north\" and the like. Here is a list "
            ++ "of common commands and a short description of each.\n\n\n"
            ++ "go [direction]\n\tGo in the indicated direction. You will be "
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
                here <- get
                let loc = value here
                    objs = objects loc
                    found = [obj | obj <- objs, name obj == head args]

                if (not . null) found
                    then do
                        fromMaybe
                            (return $ "You can't " ++ (map toLower . show) cmd ++ " the " ++ (show . name . head) found ++ ".")
                            (M.lookup cmd (commands $ head found))
                    else return $ "What " ++ head args ++ "?"
            else return "what object?"
executeCommand (Command cmd _) =
    return $ "<error: " ++ show cmd ++ " is not a supported command. this is a bug>"
