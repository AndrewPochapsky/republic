module ExhaustiveMap
    ( ExhaustiveMap
    , ExhaustiveMapClass (lookup)
    , ExhaustiveMapError
    , initExhaustiveMap
    , mkExhaustiveMap
    ) where
import qualified Data.Map as Map
import           IntoMap  (IntoMap (intoMap))
import           Utils    (expectJust)

class ExhaustiveMapClass m k v
    where lookup :: (Ord k, IntoMap m k v) => k -> m -> v

instance IntoMap (ExhaustiveMap k v) k v
    where intoMap = getMap

newtype ExhaustiveMap k v
  = ExhaustiveMap { getMap :: Map.Map k v }
  deriving (Show)

instance ExhaustiveMapClass (ExhaustiveMap k v) k v where
    lookup key eMap = expectJust $ Map.lookup key $ intoMap eMap

data ExhaustiveMapError
  = NotExhaustive
  deriving (Show)

mkExhaustiveMap :: (IntoMap m k v, Ord k, Bounded k, Enum k) => m -> Either ExhaustiveMapError (ExhaustiveMap k v)

mkExhaustiveMap intoMapType = if all (`Map.member` rawMap) allOptions
    then Right $ ExhaustiveMap rawMap
    else Left NotExhaustive
    where allOptions = [minBound .. maxBound]
          rawMap = intoMap intoMapType

initExhaustiveMap :: (Bounded k, Enum k, Ord k) => v -> ExhaustiveMap k v
initExhaustiveMap defaultValue = ExhaustiveMap $ Map.fromList (map (, defaultValue) [minBound..maxBound])
