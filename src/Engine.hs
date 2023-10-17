module Engine
    ( Direction (..)

    , Tree
    , TreeID
    , TreeZipper
    , initialize
    , node
    , leaf
    , children
    , value
    , getid
    , treeAdd
    , treeGet
    , tzHere
    , tzGoto

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
    , Place

    , wrapIntoLines
    )
  where


-- again, this warning is (alas) wrong
import {-# SOURCE #-} Engine.CommandProcessor
    ( CommandType
    )

import qualified Data.Map as M
import qualified Data.Bifunctor

import Data.Char
    ( toUpper
    )

import Control.Monad.State
    ( State
    , MonadState (get, put)
    , gets
    )

import Data.Maybe 
    ( fromMaybe
    )


data Direction = N | S | E | W | NE | NW | SE | SW 
               | D | U deriving (Show, Read, Eq, Ord)

-- | Non-binary tree. Never empty. Uses a Data.Map.Map to keep track of its children.
-- new since 0.2.0: uses `TreeID`s (`String`s) to keep track of its children
-- 
-- The idea here is to have a master list of all the nodes/leaves in a context, and 
-- use `TreeID`s to look up the child nodes. So, this module provides the `treeAdd`
-- and `treeGet` stateful monadic functions to enable insertion into the state and lookup
-- in the state respectively. See those functions for more information.
data Tree k v
    = Leaf v TreeID
    | Node v TreeID (M.Map k TreeID)

type TreeID = String

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

-- | semantic type synonym.
-- A `Thing` value should equal the name
-- of an `Object` in the current context.
type Thing = String

-- | This type carries the gamestate -- the current 
-- location and all the other locations in the "world"
type GameState = TreeZipper Direction Location
type Place = Tree Direction Location

type TreeZipper k v = (TreeStack k v, Tree k v)
type TreeStack k v = M.Map TreeID (Tree k v)

type TreeState k v r = State (TreeZipper k v) r



-- | Honestly, who wants to remember `oDescription`
-- and `lDescription` etc?
class HasDescription d where
    description :: d -> String


initialize :: Tree k v -> TreeZipper k v
initialize t = (M.fromList [(getid t, t)], t)

node :: Ord k => v -> TreeID -> M.Map k (Tree k v) -> Tree k v
node x i m = Node x i idMap
  where
    idMap = M.fromList zippedKsAndIDs
    zippedKsAndIDs = zip ks ids
    ks = M.keys m
    ids = map getid $ M.elems m -- are they necessarily returned in the same order?

-- | Abstraction layer for the "one big list" which holds
-- all the nodes for lookup by id
treeAdd :: Tree k v -> TreeState k v ()
treeAdd tr = do
    mp <- get
    put $ Data.Bifunctor.first (M.insert (getid tr) tr) mp

-- | The other end of the "one big list" abstraction
-- layer. Retrieves a node by id.
treeGet :: TreeID -> TreeState k v (Maybe (Tree k v))
treeGet i = gets $ M.lookup i . fst

tzHere :: TreeState k v (Tree k v)
tzHere = gets snd

tzGoto :: TreeID -> TreeState k v ()
tzGoto i = do
    st <- get
    target <- treeGet i
    put (fst st, fromMaybe (snd st) target)

leaf :: v -> TreeID -> Tree k v
leaf = Leaf

children :: Tree k v -> Maybe (M.Map k TreeID)
children (Node _ _ ch) = Just ch
children (Leaf _ _) = Nothing

value :: Tree k v -> v
value (Node x _ _) = x
value (Leaf x _) = x

getid :: Tree k v -> TreeID
getid (Node _ i _) = i
getid (Leaf _ i) = i

emptyLocation :: String -> Location
emptyLocation desc = location desc []

location :: String -> [Object] -> Location
location = Location

-- | useful to allow examination etc.
-- of static scenery that should not
-- be dynamically described.
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
            ++  ( concatMap ((' ' :) . (++ ".") . capitalizeFirst . description)
                -- but first we have to make sure they
                -- shouldn't be hidden
                . filter notInOrOnSomething
                -- and they actually have something to show
                -- n. b. this allows cmd-only objs to be hidden
                -- w/o a bunch of extraneous periods
                . filter (not . null . description)
                )   (objects l)

      where
        notInOrOnSomething = not . any isStrictOnOrIn . relations

        capitalizeFirst (c:str) = toUpper c :str
        capitalizeFirst [] = []

instance HasDescription Object where
    description o =
        -- take a noun phrase provided...
        oDescription o
            -- and complete the sentence based on the relations
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
