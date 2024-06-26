module Percent
    ( Percent (getValue)
    , PercentValidationError
    , mkPercent
    ) where

data PercentValidationError
  = InvalidPercentage
  | InvalidDistributionMap
  deriving (Show)

newtype Percent
  = Percent { getValue :: Double }
  deriving (Show)

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
