module ResourceGraph
    ( ResourceType (..)
    ) where

import           Data.Array
import qualified Data.Map   as Map
import qualified Data.Set   as Set

type ResourceGraph = Array ResourceType [Edge]
type Edge = (ResourceType, Int)

data ResourceType
  = Wheat
  | Bread
  | Iron
  | Coal
  | Tool
  | Consume
  deriving (Bounded, Enum, Eq, Ix, Ord, Show)

graph :: ResourceGraph
graph = mkResourceGraph $ Map.fromList [
    (Wheat, [(Bread, 1)]),
    (Iron, [(Tool, 1)]),
    (Coal, [(Tool, 1)]),
    (Bread, [(Consume, 1)]),
    (Tool, [(Consume, 1)])]

mkResourceGraph :: Map.Map ResourceType [Edge]-> ResourceGraph
mkResourceGraph m = array (minBound, maxBound) $ Map.toList m ++ map (, []) (Set.toList missingKeys)
    where allOptions :: Set.Set ResourceType = Set.fromList [minBound..maxBound]
          keys = Set.fromList $ Map.keys m
          missingKeys = allOptions `Set.difference` keys
