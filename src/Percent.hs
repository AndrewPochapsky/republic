module Percent
    ( DistributionMap (getMap)
    , Percent (getValue)
    , PercentValidationError
    , mkDistributionMap
    , mkPercent
    , oneHundredPercent
    ) where

import           Control.Monad  (join)
import           Data.Bifunctor (second)
import qualified Data.Map       as Map
import           Utils          (invertEitherList)

data PercentValidationError
  = InvalidPercentage
  | InvalidDistributionMap
  deriving (Show)

newtype Percent
  = Percent { getValue :: Double }
  deriving (Show)

newtype DistributionMap k
  = DistributionMap { getMap :: Map.Map k Percent }
  deriving (Show)

mkDistributionMap :: (Ord k) => Map.Map k Double -> Either PercentValidationError (DistributionMap k)
mkDistributionMap rawMap = join $ do
    parsedList <-  invertEitherList (map (second mkPercent) (Map.toList rawMap))
    return $ case sumOfFractions of
        1.0 -> Right $ DistributionMap (Map.fromList parsedList)
        _   -> Left InvalidDistributionMap
    where sumOfFractions = sum (Map.elems rawMap)

oneHundredPercent :: Percent
oneHundredPercent = Percent 1.0

instance Num Percent where
    (Percent x) + (Percent y) = Percent (x + y)
    (Percent x) * (Percent y) = Percent (x * y)
    abs (Percent x) = Percent (abs x)
    negate (Percent x) = Percent (negate x)
    fromInteger x = Percent (fromInteger x)
    signum _ = Percent 1.0

instance Eq Percent where
    (Percent x) == (Percent y) = x == y

mkPercent :: Double -> Either PercentValidationError Percent
mkPercent value
 | value >= 0 && value <= 1.0 = Right (Percent value)
 | otherwise = Left InvalidPercentage
