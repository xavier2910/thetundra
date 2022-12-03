module Player
    ( begin
    ) where

import Story ( start )
import Engine ( Location, Tree, Direction )

begin :: IO ()
begin = do putStrLn "The Tundra v0.1.0"
           playGame start

playGame :: Tree Direction Location -> IO ()
playGame st = putStrLn "todo"
              


              




