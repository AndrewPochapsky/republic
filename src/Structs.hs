module Structs
    ( module Structs
    ) where

data Profession
  = Farmer
  | Baker
  | Miner
  | Blacksmith
  deriving (Enum, Eq, Ord, Show)

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
  deriving (Enum, Show)

data State
  = State
      { citizens :: [Citizen]
      , time     :: Int
      }
  deriving (Show)
