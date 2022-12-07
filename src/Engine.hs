module Engine ( Direction(..)
              , Tree(..)
              , Location(..)) where 

import qualified Data.Map as M


data Direction = N | S | E | W | NE | NW | SE | SW deriving(Show, Read, Eq, Ord)

-- | Non-binary tree. Never empty. Uses a Data.Map.Map to keep track of its children.
data Tree k v = Leaf { value :: v } 
              | Node { value :: v
                     , children :: M.Map k (Tree k v) }

data Location = Location { lDescription :: String }
