{-# LANGUAGE ImportQualifiedPost #-}
module Engine (
    Direction (..),
    Tree (Leaf, Node, Jump),
    children,
    bounce,
    value,
    isNode,
    isJump,
    isLeaf,
    Location,
    objects,
    emptyLocation,
    location,
    Object,
    object,
    name,
    commands,
    Relation (..),
    HasDescription (..),
    GameState,
    wrapIntoLines,
) where

import Control.Monad.State (
    State,
 )
import Data.Char (
    toUpper,
 )
import Data.Map qualified as M
import {-# SOURCE #-} Engine.CommandProcessor (
    CommandType,
 )
import Data.Bifunctor (Bifunctor(second))

data Direction
    = N
    | S
    | E
    | W
    | NE
    | NW
    | SE
    | SW
    | D
    | U
    | IN
    | OUT
    | AWAY
    deriving (Show, Read, Eq, Ord)

-- | Non-binary tree. Never empty. Uses a Data.Map.Map to keep track of its children.
data Tree k v
    = Leaf v
    | Node v (M.Map k (Tree k v))
    | Jump v (Tree k v)

data Location = Location
    { lDescription :: String
    , objects :: [Object]
    }

data Object = Object
    { oName :: String
    -- ^ ideally should be unique, but is used by
    -- player to x, take, etc, so be careful
    , relations :: [Relation]
    , oDescription :: String
    -- ^ generally, this should be a noun phrase
    -- for compatibility with the sentence builder
    -- in `Object`'s `HasDescription` instance
    , commands :: M.Map CommandType (State GameState String)
    -- ^ the idea here is that this is a general
    -- interface for object commands. I am currently
    -- unsure of how helpful it will be or even
    -- how best to implement such an interface
    }

{- | used to construct the description of
 an Object
-}
data Relation
    = -- | represents when an `Object` is strictly 'on'
      -- something and should only be visible on closer
      -- examination.
      OnStrict Thing
    | -- | represents when an `Object` should be described
      -- as 'on' something but strictly isn't 'on' that
      -- thing
      OnLoose Thing
    | -- | represents when an `Object` is strictly 'in'
      -- something and should only be visible on closer
      -- examination.
      InStrict Thing
    | -- | represents when an `Object` should be described
      -- as 'in' something but strictly isn't 'in' that
      -- thing
      InLoose Thing
    | -- | a VerbPhrase value specifies the verb phrase
      -- to be used with the rest of the relations when
      -- constructing descriptions, eg. 'sits disquietingly'
      VerbPhrase String

{- | semantic type synonym
 a `Thing` value shuold equal the name
 of an `Object` in the current context
-}
type Thing = String

type GameState = Tree Direction Location

{- | Honestly, who wants to remember `oDescription`
 and `lDescription` etc?
-}
class HasDescription d where
    description :: d -> String

children :: Tree k v -> Maybe (M.Map k (Tree k v))
children (Node _ ch) = Just ch
children _ = Nothing

value :: Tree k v -> v
value (Node x _) = x
value (Jump x _) = x
value (Leaf x) = x

bounce :: Tree k v -> Maybe (Tree k v)
bounce (Jump _ nxt) = Just nxt
bounce _ = Nothing

isNode :: Tree k v -> Bool
isNode (Node _ _) = True
isNode _ = False

isJump :: Tree k v -> Bool
isJump (Jump _ _) = True
isJump _ = False

isLeaf :: Tree k v -> Bool
isLeaf (Leaf _) = True
isLeaf _ = False

emptyLocation :: String -> Location
emptyLocation desc = location desc []

location :: String -> [Object] -> Location
location = Location

-- | usage: `object name relations oDescription commands`
object :: String -> [Relation] -> String -> M.Map CommandType (State GameState String) -> Object
object = Object

name :: Object -> String
name = oName

isStrictOnOrIn :: Relation -> Bool
isStrictOnOrIn (OnStrict _) = True
isStrictOnOrIn (InStrict _) = True
isStrictOnOrIn _ = False

{- | determines whether a `Relation` is an
 on or in, ignoring whether it should be shown
 outside the `Thing`'s context
-}
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

{- | takes a line length (in characters) and
 wraps the input string at that length
-}
wrapIntoLines :: Int -> String -> String
wrapIntoLines l str =
    if length str > l -- end case if the line is too short
        && l > 0 -- and idiot-proof it
        -- take this line, a newline, and the rest, wrapped
        then
            thisLine
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
        -- sure we don't cut off the \'-\' at the end
        -- of the line
        | otherwise = length thisLine - 1

instance HasDescription Location where
    description l =
        -- get the place description...
        lDescription l
            -- then describe the objects there:
            -- format them nicely into sentences & stitch them together
            ++ ( concatMap ((' ' :) . (++ ".") . capitalizeFirst . description)
                    -- but first we have to make sure they
                    -- shouldn't be hidden
                    . filter notInOrOnSomething
                    -- and they actually have something to show
                    -- n. b. this allows cmd-only objs to be hidden
                    -- w/o a bunch of extraneous periods
                    . filter (not . null . description)
               )
                (objects l)
      where
        notInOrOnSomething = not . any isStrictOnOrIn . relations

        capitalizeFirst (c : str) = toUpper c : str
        capitalizeFirst [] = []

instance HasDescription Object where
    description o =
        -- take a noun phrase provided...
        oDescription o
            -- and complete the sentence based on the relations
            -- and complete the sentence based on the relations
            -- present (if any)
            ++ if not . any isLooseOrStrictOnOrIn $ relations o
                then "" -- if there aren't any, we give up (it's probably
                -- in the player's inventory, and there isn't
                -- much to say. Just imagine you query your inven-
                -- tory and the computer solemnly informs you that your
                -- waterbottle (in your pack) is sitting on a desk???)
                else -- if, on the other hand, there are relations, we pretty
                -- them up:
                -- first find the (first) verb (no compound predicates, sorry),
                -- and put a space in front of it

                    ((' ' :) . relationToString . head . filter isVerbPhrase) (relations o)
                        -- and then (since the hidden cases were taken care of by
                        -- `Location`'s `description` implementation) we get all
                        -- the on's or in's, space them out, and stitch them
                        -- together
                        ++ concatMap ((' ' :) . relationToString) (filter isLooseOrStrictOnOrIn $ relations o)

instance (Ord k) => Functor (Tree k) where
    fmap f (Leaf x) = Leaf $ f x
    fmap f (Jump x nxt) = Jump (f x) (fmap f nxt)
    fmap f (Node x childMap) = Node (f x) (M.fromList $ map (second (fmap f)) $ M.toList childMap)

instance Eq Location where
    oneLoc == otherLoc = description oneLoc == description otherLoc

instance (Eq v) => Eq (Tree k v) where
    oneTree == another = value oneTree == value another
