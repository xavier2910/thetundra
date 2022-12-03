module Engine ( Direction(..)
              , Tree(..)
              , Location(..)) where 

import qualified Data.Map as M


data Direction = N | S | E | W | NE | NW | SE | SW deriving(Show, Eq, Ord)

-- | Non-binary tree. Never empty. Uses a Data.Map.Map to keep track of its children.
data Tree k v = Leaf v 
              | Node v (M.Map k (Tree k v))

data Location = Location { lDescription :: String }
