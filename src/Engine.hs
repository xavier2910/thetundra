module Engine 
    ( Direction (..)

    , Tree (Leaf, Node)
    , children
    , value 

    , Location
    , objects
    , emptyLocation
    , location

    , Object
    , object
    , commandOnlyObject
    , name
    , commands

    , Relation (..)

    , HasDescription (..)

    , GameState

    , wrapIntoLines
    ) 
  where


-- again, this warning is (alas) wrong
import {-# SOURCE #-} Engine.CommandProcessor 
    ( CommandType (..)
    )

import qualified Data.Map as M
import Data.Char 
    ( toUpper 
    )

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
    -- | ideally should be unique, but is used by
    -- player to x, take, etc, so be careful
    { oName :: String
    , relations :: [Relation]
    -- | generally, this should be a noun phrase
    -- for compatibility with the sentence builder
    -- in `Object`'s `HasDescription` instance
    , oDescription :: String
    -- | the idea here is that this is a general
    -- interface for object commands. I am currently
    -- unsure of how helpful it will be or even
    -- how best to implement such an interface
    , commands :: M.Map CommandType String
    }

-- | used to construct the description of
-- an Object    
data Relation
    -- | represents when an `Object` is strictly 'on'
    -- something and should only be visible on closer
    -- examination.
    = OnStrict Thing
    -- | represents when an `Object` should be described
    -- as 'on' something but strictly isn't 'on' that
    -- thing
    | OnLoose Thing
    -- | represents when an `Object` is strictly 'in'
    -- something and should only be visible on closer
    -- examination.
    | InStrict Thing
    -- | represents when an `Object` should be described
    -- as 'in' something but strictly isn't 'in' that
    -- thing
    | InLoose Thing
    -- | a VerbPhrase value specifies the verb phrase 
    -- to be used with the rest of the relations when
    -- constructing descriptions, eg. 'sits disquietingly' 
    | VerbPhrase String

-- | semantic type synonym 
-- a `Thing` value shuold equal the name
-- of an `Object` in the current context
type Thing = String

type GameState = Tree Direction Location


-- | Honestly, who wants to remember `oDescription`
-- and `lDescription` etc?
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

-- | useful to allow examination etc.
-- of static scenery that should not
-- be dynamically described
commandOnlyObject :: String -> M.Map CommandType String -> Object
commandOnlyObject s = Object s [] ""

object :: String -> [Relation] -> String -> M.Map CommandType String -> Object
object = Object

name :: Object -> String
name = oName

isStrictOnOrIn :: Relation -> Bool
isStrictOnOrIn (OnStrict _) = True
isStrictOnOrIn (InStrict _) = True
isStrictOnOrIn _ = False

-- | determines whether a `Relation` is an
-- on or in, ignoring whether it should be shown
-- outside the `Thing`'s context
isLooseOrStrictOnOrIn :: Relation -> Bool
isLooseOrStrictOnOrIn (OnStrict _) = True
isLooseOrStrictOnOrIn (OnLoose _) = True
isLooseOrStrictOnOrIn (InStrict _) = True
isLooseOrStrictOnOrIn (InLoose _) = True
isLooseOrStrictOnOrIn _ = False

isVerbPhrase :: Relation -> Bool
isVerbPhrase (VerbPhrase _) = True
isVerbPhrase _ = False

relationToString :: Relation -> String
relationToString (OnStrict str) = "on " ++ str 
relationToString (OnLoose str) = "on " ++ str
relationToString (InStrict str) = "in " ++ str
relationToString (InLoose str) = "in " ++ str
relationToString (VerbPhrase str) = str

-- | takes a line length (in characters) and
-- wraps the input string at that length
wrapIntoLines :: Int -> String -> String
wrapIntoLines l str = if length str > l -- end case if the line is too short 
                      && l > 0 -- and idiot-proof it
    -- take this line, a newline, and the rest, wrapped
    then thisLine 
        ++ "\n" 
        ++ wrapIntoLines l (drop dropLength str)
    else str

  where 
    thisLine
        -- if we have a newline in the next l characters,
        -- 'thisLine' should only go up to that newline
        | '\n' `elem` take_l_str = takeWhile (/= '\n') str
        -- if there's a space in this line, take the last
        -- space before the character limit.
        -- we only worry about space-breaking if there
        -- isn't already a newline to do the work for us
        | ' ' `elem` take_l_str = reverse $ dropWhile (/= ' ') (reverse take_l_str)
        -- if there's no space, we just stick a dash
        -- on it
        | otherwise = take_l_str ++ "-"
        
    take_l_str = take l str

    dropLength
        -- if we've a newline, cut it off, it was
        -- left out of 'thisLine'
        | '\n' `elem` take_l_str = length thisLine + 1
        -- if we have a space to split on, there's nothing
        -- to worry about
        | ' ' `elem` take_l_str = length thisLine
        -- if there wasn't a space, we need to make
        -- sure we don't cut of the \'-\' at the end
        -- of the line
        | otherwise = length thisLine - 1

instance HasDescription Location where
    description l =
        -- get the place description...
        lDescription l
            -- then describe the objects there:
                -- format them nicely into sentences & stitch them together
            ++  ( concatMap ((' ' :) . (++ "."). capitalizeFirst . description) 
                -- but first we have to make sure they
                -- shouldn't be hidden
                . filter notInOrOnSomething ) 
                    (objects l)

      where
        notInOrOnSomething = not . any isStrictOnOrIn . relations

        capitalizeFirst (c:str) = toUpper c :str
        capitalizeFirst [] = []

instance HasDescription Object where
    description o = 
        -- take a noun phrase provided...
        oDescription o
            -- and completes the sentence based on the relations
            -- present (if any)
            ++ if not . any isLooseOrStrictOnOrIn $ relations o
                then "" -- if there aren't any, we give up (it's probably
                        -- in the player's inventory, and there isn't
                        -- much to say. Just imagine you query your inven-
                        -- tory and the computer solemnly informs you that your
                        -- waterbottle (in your pack) is sitting on a desk???)

                    -- if, on the other hand, there are relations, we pretty
                    -- them up:
                    -- first find the (first) verb (no compound predicates, sorry),
                    -- and put a space in front of it
                else ((' ':) . relationToString . head . filter isVerbPhrase) (relations o)
                    -- and then (since the hidden cases were taken care of by
                    -- `Location`'s `description` implementation) we get all
                    -- the on's or in's, space them out, and stitch them
                    -- together
                    ++ concatMap ((' ':) . relationToString) (filter isLooseOrStrictOnOrIn $ relations o)
