module Player (
    begin,
) where

import Control.Monad (
    when,
 )
import Control.Monad.State (
    runState,
 )
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Engine (
    GameState,
    Tree (Leaf),
    bounce,
    description,
    emptyLocation,
    isJump,
    isLeaf,
    isNode,
    value,
    wrapIntoLines,
 )
import Engine.CommandProcessor (
    executeCommand,
    stringToCommand,
 )
import Story (
    start,
 )
import System.Console.ANSI (
    Color (Green, White),
    ColorIntensity (Vivid),
    ConsoleIntensity (BoldIntensity),
    ConsoleLayer (Foreground),
    SGR (Reset, SetColor, SetConsoleIntensity),
    setSGR,
 )
import System.IO (
    hFlush,
    stdout,
 )

-- | global print line length limit.
lineLength :: Int
lineLength = 75

{- | For those curious, the Ln's stand for all the newlines inserted
 in various places
-}
putLnWrappedStrLnLn :: Int -> String -> IO ()
putLnWrappedStrLnLn lnLength msg = putStrLn . wrapIntoLines lnLength $ "\n" ++ msg ++ "\n"

begin :: IO ()
begin = do
    putStrLn "The Tundra v0.1.2\n by Xavier\n\nType a command, or type '?' for help.\n'q' exits."
    putStrLn "Please note commands are not case-sensitive.\n"
    let curLoc = value start

    putLnWrappedStrLnLn lineLength $ description curLoc
    playGame start

playGame :: GameState -> IO ()
playGame st
    | isNode st = do
        setSGR
            [ SetConsoleIntensity BoldIntensity
            , SetColor Foreground Vivid Green
            ]

        putStr "> "
        setSGR [SetColor Foreground Vivid White]
        hFlush stdout
        command <- getLine
        setSGR [Reset]

        when
            (null command || head command /= 'q')
            ( case stringToCommand command of
                Just cmd -> do
                    let (msg, nst) = runState (executeCommand cmd) st
                    putLnWrappedStrLnLn lineLength msg
                    playGame nst
                Nothing -> do
                    putStrLn "\nbad command :(\n"
                    playGame st
            )
    | isJump st = do
        setSGR
            [ SetConsoleIntensity BoldIntensity
            , SetColor Foreground Vivid Green
            ]
        putStr "press enter to continue . . . "
        setSGR [SetColor Foreground Vivid White]
        hFlush stdout
        _ <- getLine

        let nst = fromMaybe (Leaf $ emptyLocation "playGame: isNode/isJump/isLeaf aren't working right") $ bounce st
        setSGR [Reset]
        putLnWrappedStrLnLn lineLength $ description $ value nst

        playGame nst
    | isLeaf st = do
        setSGR
            [ SetConsoleIntensity BoldIntensity
            , SetColor Foreground Vivid Green
            ]
        putStr "Play again? (y/n)> "
        setSGR [SetColor Foreground Vivid White]
        hFlush stdout
        reply <- getLine

        when
            ((not . null) reply && (toLower . head) reply == 'y')
            ( do
                setSGR [Reset]
                putLnWrappedStrLnLn lineLength $ description $ value start
                playGame start
            )
    | otherwise = error "playGame: somehow you produced a Tree which is not a Node, Jump, or Leaf"
