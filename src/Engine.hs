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

    , value 

    , wrapIntoLines
    ) 
  where

import qualified Data.Map as M
import Data.Char 
    ( toUpper )

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

-- | used to construct the description of
-- an Object    
data Relation
    = On Thing
    | OnButShow Thing
    | In Thing
    | InButShow Thing
    -- | a Verb value specifies the verb to be used with the relations, eg. 'sits'
    | Verb String

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

isOnOrInIndescriminate :: Relation -> Bool
isOnOrInIndescriminate (On _) = True
isOnOrInIndescriminate (OnButShow _) = True
isOnOrInIndescriminate (In _) = True
isOnOrInIndescriminate (InButShow _) = True
isOnOrInIndescriminate _ = False

isVerb :: Relation -> Bool
isVerb (Verb _) = True
isVerb _ = False

relationToString :: Relation -> String
relationToString (On str) = "on " ++ str 
relationToString (OnButShow str) = "on " ++ str
relationToString (In str) = "in " ++ str
relationToString (InButShow str) = "in " ++ str
relationToString (Verb str) = str
-- this is here in case I add a relation & forget to add it to this function
relationToString _ = "<error: unrecognized relation - this is a bug>"

-- | takes a line length (in characters) and
-- wraps the input string at that length
wrapIntoLines :: Int -> String -> String
wrapIntoLines l str = if length str > l && l > 0
    then thisLine 
        ++ "\n" 
        ++ wrapIntoLines l 
            (drop 
                -- compensate for the '-' added to thisLine if the word was too long
                (length thisLine - if ' ' `elem` take l str then 0 else 1) 
                str)
    else str
  where thisLine = if ' ' `elem` take l str 
        then reverse $ dropWhile (/= ' ') (reverse $ take l str)
        else take l str ++ "-"

instance HasDescription Location where
    description l =
        lDescription l
            ++ 
                ( concatMap ((' ' :) . (++ "."). capitalizeFirst . description) 
                . filter notInOrOnSomething ) 
            (objects l)

      where
        notInOrOnSomething = not . any isOnOrIn . relations

        capitalizeFirst (c:str) = toUpper c :str
        capitalizeFirst [] = []

instance HasDescription Object where
    description o = 
        oDescription o
            ++ if not . any isOnOrInIndescriminate $ relations o
                then ""
                else ((' ':) . relationToString . head . filter isVerb) (relations o)
                    ++ concatMap ((' ':) . relationToString) (filter isOnOrInIndescriminate $ relations o)
