module Player 
    ( begin 
    ) 
  where


import Engine
    ( description
    , value
    , wrapIntoLines
    , GameState
    )

import Engine.CommandProcessor
    ( stringToCommand
    , executeCommand
    )

import Story 
    ( start 
    )

import System.IO 
    ( hFlush
    , stdout 
    )

import Control.Monad
    ( when
    )

import Control.Monad.State 
    ( runState
    )


-- | global print line length limit.
lineLength :: Int
lineLength = 75


begin :: IO ()
begin = do
    putStrLn "The Tundra v0.1.0\n by Xavier\n\nType a command, or type '?' for help.\nBlank line or 'q' exits."
    putStrLn "I guess you could type Ctrl + C, too, but that's bad form.\nIt'll give you an ugly uncaught exception message."
    putStrLn "Please note commands are not case-sensitive.\n"
    let msg = description $ value start

    putStrLn . wrapIntoLines lineLength $ "\n" ++ msg ++ "\n"
    playGameCmd start


playGameCmd :: GameState -> IO ()
playGameCmd st = do
    putStr "> "
    hFlush stdout
    command <- getLine
    
    when ((not . null) command && head command /= 'q')
        (do
            case stringToCommand command of 
                Just cmd -> do
                    let (msg, nst) = runState (executeCommand cmd) st
                    putStrLn . wrapIntoLines lineLength $ "\n" ++ msg ++ "\n"
                    playGameCmd nst

                Nothing -> do
                    putStrLn "\nbad command :(\n"
                    playGameCmd st
        )

