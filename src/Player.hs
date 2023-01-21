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

-- | For those curious, the Ln's stand for all the newlines inserted
-- in various places
putLnWrappedStrLnLn :: Int -> String -> IO ()
putLnWrappedStrLnLn lnLength msg = putStrLn . wrapIntoLines lnLength $ "\n" ++ msg ++ "\n"


begin :: IO ()
begin = do
    putStrLn "The Tundra v0.1.0\n by Xavier\n\nType a command, or type '?' for help.\n'q' exits."
    putStrLn "Please note commands are not case-sensitive.\n"
    let msg = description $ value start

    putLnWrappedStrLnLn lineLength msg 
    playGame start


playGame :: GameState -> IO ()
playGame st = do
    putStr "> "
    hFlush stdout
    command <- getLine
    
    when (null command || head command /= 'q')
        (do
            case stringToCommand command of 
                Just cmd -> do
                    let (msg, nst) = runState (executeCommand cmd) st
                    putLnWrappedStrLnLn lineLength msg 
                    playGame nst

                Nothing -> do
                    putStrLn "\nbad command :(\n"
                    playGame st
        )

