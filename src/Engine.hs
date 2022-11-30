module Engine ( ) where 

import qualified Data.Map as M


data Direction = N | S | E | W | NE | NW | SE | SW deriving(Show, Eq)

-- | Non-binary tree. Never empty. Uses a Data.Map.Map to keep track of its children.
data Tree k v = Leaf v 
              | Node v (M.Map k v)
