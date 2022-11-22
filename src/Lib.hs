module Lib
    ( playGame
    ) where

import qualified Data.Map as Map

data Direction = N | S | E | W | NE | NW | SE | SW
data Location = Location { description :: String
                         , connections :: Map.Map Direction Location }

playGame :: IO ()
playGame = do putStrLn "The Tundra v1.0.0"


              




