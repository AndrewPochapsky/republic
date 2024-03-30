module DistributionMap where

import           Control.Monad  (join)
import           Data.Bifunctor (second)
import qualified Data.Map       as Map
import           ExhaustiveMap  (ExhaustiveMap, ExhaustiveMapClass (lookup),
                                 ExhaustiveMapError, mkExhaustiveMap)
import           IntoMap        (IntoMap (intoMap))
import           Percent        (Percent, PercentValidationError, mkPercent)
import           Utils          (invertEitherList)

data DistributionMapError
  = InvalidDistributionMap
  | PercentError PercentValidationError
  | ExhaustiveMapError ExhaustiveMapError
  deriving (Show)

convertPercentError :: Either PercentValidationError a -> Either DistributionMapError a
convertPercentError eitherValue = case eitherValue of
    Left err      -> Left (PercentError err)
    (Right value) -> Right value

convertExhaustiveMapError :: Either ExhaustiveMapError a -> Either DistributionMapError a
convertExhaustiveMapError eitherValue = case eitherValue of
    Left err      -> Left (ExhaustiveMapError err)
    (Right value) -> Right value

-- Enforces that all values add up to 1.
newtype DistributionMap k
  = DistributionMap { getMap :: Map.Map k Percent }
  deriving (Show)

instance IntoMap (DistributionMap k) k Percent
    where intoMap = getMap

newtype ExhaustiveDistributionMap k
  = ExhaustiveDistributionMap { getExhaustiveMap :: ExhaustiveMap k Percent }
  deriving (Show)

instance IntoMap (ExhaustiveDistributionMap k) k Percent
    where intoMap eMap = intoMap $ getExhaustiveMap eMap

instance ExhaustiveMapClass (ExhaustiveDistributionMap k) k Percent where
    lookup key m = ExhaustiveMap.lookup key (getExhaustiveMap m)

mkDistributionMap :: (Ord k, IntoMap m k Double) => m -> Either DistributionMapError (DistributionMap k)
mkDistributionMap intoMapThing = join $ do
    parsedList <- convertPercentError $ invertEitherList (map (second mkPercent) (Map.toList rawMap))
    return $ case sumOfFractions of
        1.0 -> Right $ DistributionMap (Map.fromList parsedList)
        _   -> Left InvalidDistributionMap
    where
        rawMap = intoMap intoMapThing
        sumOfFractions = sum (Map.elems rawMap)

mkExhaustiveDistributionMap :: forall m k. (IntoMap m k Double, Ord k, Bounded k, Enum k) => m -> Either DistributionMapError (ExhaustiveDistributionMap k)
mkExhaustiveDistributionMap intoMapThing = do
    distributionMap :: DistributionMap k <- mkDistributionMap intoMapThing
    exhaustiveMap <- convertExhaustiveMapError $ mkExhaustiveMap distributionMap
    return $ ExhaustiveDistributionMap exhaustiveMap

toList :: (Ord k) => DistributionMap k -> [(k, Percent)]
toList m = Map.toList $ getMap m
