module Engine ( Location
              , Vehicle
              , Furniture
              , NPC
              , Role(defaultInventory)
              , Item
              , Inventory) where


data Location = Location { lDescription :: String
                         , isPlayerHere :: Bool
                         , children :: Inventory } deriving(Show, Eq)

data Vehicle = Vehicle { vDescription :: String 
                       , passengers :: Inventory } deriving(Show, Eq)
newtype Furniture = Furniture { fDescription :: String } deriving(Show, Eq)

data NPC r =  NPC { name :: String
                  , role :: r
                  , nDescription :: String
                  , nInventory :: Inventory } deriving(Show, Eq)

class (Show r) => Role r where
    defaultInventory :: r -> Inventory

newtype Item = Item { iDescription :: String } deriving(Show, Eq)
type Inventory = [Item]