module Engine ( Direction(..)

              , Tree(Leaf, Node)

              , Location
              , objects
              , emptyLocation
              , location

              , Object
              , object

              , HasDescription(..)


              , children
              , value) where 

import qualified Data.Map as M


data Direction = N | S | E | W | NE | NW | SE | SW deriving(Show, Read, Eq, Ord)

-- | Non-binary tree. Never empty. Uses a Data.Map.Map to keep track of its children.
data Tree k v = Leaf v 
              | Node v (M.Map k (Tree k v))

data Location = Location { lDescription :: String
                         , objects :: [Object] }

newtype Object = Object {oDescription :: String}


class HasDescription d where
       description ::  d -> String


children :: Tree k v -> Maybe (M.Map k (Tree k v))
children (Node _ ch) = Just ch
children (Leaf _) = Nothing

value :: Tree k v -> v
value (Node x _) = x
value (Leaf x) = x

emptyLocation :: String -> Location
emptyLocation = flip Location [] 

location :: String -> [Object] -> Location
location = Location

object :: String -> Object
object = Object


instance HasDescription Location where
       description = lDescription

instance HasDescription Object where
       description = oDescription

