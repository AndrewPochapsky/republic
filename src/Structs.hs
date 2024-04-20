module Structs
    ( module Structs
    ) where
import           ExhaustiveMap (ExhaustiveMap)
import           ResourceGraph (ResourceType)

data Profession
  = Farmer
  | Baker
  | Miner
  | Blacksmith
  deriving (Bounded, Enum, Eq, Ord, Show)

data Citizen
  = Gold
      { age :: Int
      }
  | Silver
      { age :: Int
      }
  | Iron
      { age        :: Int
      , profession :: Profession
      }
  deriving (Show)

data CitizenType
  = GoldType
  | SilverType
  | IronType
  deriving (Bounded, Enum, Eq, Ord, Show)

type ProductionInfo = (ResourceType, Int)
data State
  = State
      { citizens                :: [Citizen]
      , time                    :: Int
      , resources               :: ExhaustiveMap ResourceType Int
      , professionProductionMap :: ExhaustiveMap Profession ProductionInfo
      }
  deriving (Show)
