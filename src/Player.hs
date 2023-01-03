module Player 
    ( begin ) where

import Engine 
    ( Direction
    , Location
    , Tree
    , children
    , description
    , value )
import Story 
    ( start )

import Data.Char 
    ( toUpper )
import qualified Data.Map as M
import System.IO 
    ( hFlush
    , stdout )

begin :: IO ()
begin = do
    putStrLn "The Tundra v0.1.0"
    let msg = description $ value start

    putStrLn $ "\n" ++ msg ++ "\n"
    playGame start

playGame :: Tree Direction Location -> IO ()
playGame st = do
    putStr "> "
    hFlush stdout
    command <- getLine


    case reads $ map toUpper command :: [(Direction, String)] of

        [(dir, _)] -> case children st of
            Just chs -> case M.lookup dir chs of

                Just tree -> do
                    let msg = description $ value tree
                    putStrLn $ "\n" ++ msg ++ "\n"
                    playGame tree

                Nothing -> do
                    putStrLn "\nYou can't go that way :(\n"
                    playGame st

            Nothing -> putStrLn "GAME OVER :(" -- todo: dead end case should be caught before
                                               --       this command cycle, making this code
                                               --       unreachable
            
        _ -> do
            putStrLn "\nBad command :(\n"
            playGame st
