module Engine 
    ( Direction (..)

    , Tree (Leaf, Node)

    , Location
    , objects
    , emptyLocation
    , location

    , Object
    , object
    , name

    , Relation (..)

    , HasDescription (..)

    , children

    , value ) where

import Data.Map qualified as M

data Direction = N | S | E | W | NE | NW | SE | SW deriving (Show, Read, Eq, Ord)

-- | Non-binary tree. Never empty. Uses a Data.Map.Map to keep track of its children.
data Tree k v
    = Leaf v
    | Node v (M.Map k (Tree k v))

data Location = Location
    { lDescription :: String
    , objects :: [Object]
    }

data Object = Object
    { oName :: String
    , relations :: [Relation]
    , oDescription :: String
    }

data Relation
    = On Thing
    | OnButShow Thing
    | In Thing
    | InButShow Thing

type Thing = String


class HasDescription d where
    description :: d -> String


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

object :: String -> [Relation] -> String -> Object
object = Object

name :: Object -> String
name = oName

isOnOrIn :: Relation -> Bool
isOnOrIn (On _) = True
isOnOrIn (In _) = True
isOnOrIn _ = False


instance HasDescription Location where
    description l =
        lDescription l
            ++ (concatMap ((' ' :) . description) . filter notInOrOnSomething) (objects l)
      where
        notInOrOnSomething = not . any isOnOrIn . relations

instance HasDescription Object where
    description = oDescription
