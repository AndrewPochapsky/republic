--module Structs (Profession, Citizen, State, ProfessionDistribution, StartingConfiguration(StartingConfiguration), startingSize, goldPercent) where
module Structs (module Structs) where

import qualified Data.Map as Map

data Profession = Farmer | Baker | Miner | Blacksmith deriving (Show, Ord, Eq, Bounded, Enum)
data Citizen = Gold { age :: Int } | Silver { age :: Int } | Iron { age :: Int, profession :: Profession } deriving Show
data State = State { citizens :: [Citizen], time :: Int } deriving Show
newtype ProfessionDistribution = ProfessionDistribution { getMap::Map.Map Profession Double }
data StartingConfiguration = StartingConfiguration {startingSize :: Int, goldPercent :: Double, silverPercent :: Double, ironPercent :: Double, ironProfessionDistribution :: ProfessionDistribution }
