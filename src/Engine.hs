module Engine ( Location ( lDescription, isPlayerAt, inhabitants )
              , Inhabitants
              , Vehicle ( vDescription, passengers )
              , Furniture ( fDescription )
              , NPC ( name, role, nDescription, nInventory )
              , Role ( defaultInventory )
              , Item
              , Inventory) where  


data Location r = Location { lDescription :: String
                           , isPlayerAt :: Bool
                           , inhabitants :: Inhabitants r } deriving(Show, Eq)

type Inhabitants r = ([Furniture], [Vehicle], [NPC r], Inventory)

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