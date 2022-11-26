module Engine ( Location ( lDescription
                         , isPlayerAt
                         , inhabitants
                         , getConnectedLocations )
              , Inhabitants
              , Direction (..)
              , Vehicle ( vDescription
                        , passengers )
              , Furniture (fDescription)
              , NPC ( name
                    , role
                    , nDescription
                    , nInventory )
              , Role (defaultInventory)
              , Item
              , Inventory) where 

import qualified Data.Map as M 

-- | represents a location and its attached locations; therefore the whole world by recursion
data Location r = Location { lDescription :: String
                           , isPlayerAt :: Bool
                           , inhabitants :: Inhabitants r
                           , getConnectedLocations :: M.Map Direction (Location r) } deriving(Eq)

type Inhabitants r = ([Furniture], [Vehicle], [NPC r], Inventory)

data Direction = N | S |E | W | NE | NW | SE | SW deriving(Show, Eq)

newtype Furniture = Furniture { fDescription :: String } deriving(Show, Eq)

data Vehicle = Vehicle { vDescription :: String 
                       , passengers :: Inventory } deriving(Show, Eq)

data NPC r =  NPC { name :: String
                  , role :: r
                  , nDescription :: String
                  , nInventory :: Inventory } deriving(Show, Eq)

class (Show r) => Role r where
    defaultInventory :: r -> Inventory

newtype Item = Item { iDescription :: String } deriving(Show, Eq)

type Inventory = [Item]



instance (Role r) => Show (Location r) where
    show loc = "Location { lDescription = " ++ show (lDescription loc) 
                     ++ ", isPlayerAt = " ++ show (isPlayerAt loc)
                     ++ ", inhabitants = " ++ show (inhabitants loc)
                     ++ ", getConnectedLocations = fromList " ++ show (M.toList $ getConnectedLocations loc)

