module Percent
    ( Percent (getValue)
    , PercentValidationError
    , mkPercentFrom
    , oneHundredPercent
    ) where

data PercentValidationError
  = InvalidPercentage
  deriving (Show)

newtype Percent
  = Percent { getValue :: Double }
  deriving (Show)

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

mkPercentFrom :: (a -> Double) -> a -> Either PercentValidationError Percent
mkPercentFrom toDouble value = validateDoubleAsPercent $ toDouble value

validateDoubleAsPercent :: Double -> Either PercentValidationError Percent
validateDoubleAsPercent value
 | value >= 0 && value <= 1.0 = Right (Percent value)
 | otherwise = Left InvalidPercentage
