module Player
    ( begin
    ) where

import Story ( start )
import Engine ( Location(Location), Tree(children, value), Direction )

import Data.Char ( toUpper )
import qualified Data.Map as M
import System.IO (stdout, hFlush)


begin :: IO ()
begin = do putStrLn "The Tundra v0.1.0"
           let msg = case value start of Location message -> message
                                   
           putStrLn $ "\n" ++ msg ++ "\n"
           playGame start

playGame :: Tree Direction Location -> IO ()
playGame st = do putStr "> "
                 hFlush stdout
                 command <- getLine

                 -- n. b. this does not check against Leaf's
                 case reads $ map toUpper command :: [(Direction, String)] of [(dir, _)] -> case M.lookup dir $ children st of Just tree -> do let msg = case value tree of Location message -> message
                                                                                                                                               putStrLn $ "\n" ++ msg ++ "\n"
                                                                                                                                               playGame tree
                                                                                                                               Nothing -> do putStrLn "\nYou can't go that way :(\n"
                                                                                                                                             playGame st
                                                                              _ -> do putStrLn "\nBad command :(\n"
                                                                                      playGame st
              


              




