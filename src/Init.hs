{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
module Init
    ( GoldPercent (..)
    , IronPercent (..)
    , SilverPercent (..)
    , StartingConfiguration
    , StartingSize (..)
    , generateStartingState
    , mkStartingConfiguration
    ) where

import           Control.Monad   (join)
import qualified Data.Map        as Map
import           Numeric.Natural
import           Structs         (Citizen (..), Profession, State (..))

data ValidationError
  = InvalidProfessionDistribution
  | InvalidPercentage
  | PercentagesMustAddToOne
  deriving (Show)

class ExpectedPercent a where validate :: a -> Either ValidationError Percent

instance ExpectedPercent GoldPercent where
    validate (GoldPercent x)
     | x > 0 && x < 1.0 = Right (Percent x)
     | otherwise = Left InvalidPercentage

instance ExpectedPercent SilverPercent where
    validate (SilverPercent x)
     | x > 0 && x < 1.0 = Right (Percent x)
     | otherwise = Left InvalidPercentage

instance ExpectedPercent IronPercent where
    validate (IronPercent x)
     | x > 0 && x < 1.0 = Right (Percent x)
     | otherwise = Left InvalidPercentage

newtype StartingSize
  = StartingSize Natural
  deriving (Show)

newtype GoldPercent
  = GoldPercent Double
  deriving (Show)

newtype SilverPercent
  = SilverPercent Double
  deriving (Show)

newtype IronPercent
  = IronPercent Double
  deriving (Show)

newtype ProfessionDistribution
  = ProfessionDistribution { getMap :: Map.Map Profession Double }
  deriving (Show)

newtype Percent
  = Percent Double
  deriving (Show)

instance Num Percent where
    (Percent x) + (Percent y) = Percent (x + y)
    (Percent x) * (Percent y) = Percent (x * y)
    abs (Percent x) = Percent (abs x)
    negate (Percent x) = Percent (negate x)
    fromInteger x = Percent (fromInteger x)
    signum _ = Percent 1

instance Eq Percent where
    (Percent x) == (Percent y) = x == y

mkProfessionDistribution :: Map.Map Profession Double -> Either ValidationError ProfessionDistribution
mkProfessionDistribution rawMap
    | sumOfFractions == 1.0 = Right (ProfessionDistribution rawMap)
    | otherwise = Left InvalidProfessionDistribution
    where sumOfFractions = sum (Map.elems rawMap)

data StartingConfiguration
  = StartingConfiguration
      { startingSize               :: StartingSize
      , goldPercent                :: Percent
      , silverPercent              :: Percent
      , ironPercent                :: Percent
      , ironProfessionDistribution :: ProfessionDistribution
      }
  deriving (Show)

mkStartingConfiguration :: (ExpectedPercent GoldPercent, ExpectedPercent SilverPercent, ExpectedPercent IronPercent) =>
    StartingSize -> GoldPercent -> SilverPercent -> IronPercent -> Map.Map Profession Double -> Either ValidationError StartingConfiguration

mkStartingConfiguration startingSize' goldPercent' silverPercent' ironPercent' professionDistributionMap' =
     join $ do
        gold <- validate goldPercent'
        silver <- validate silverPercent'
        iron <- validate ironPercent'
        professionDistribution <- mkProfessionDistribution professionDistributionMap'
        return $ internalMake startingSize' gold silver iron professionDistribution
    where internalMake :: StartingSize -> Percent -> Percent -> Percent -> ProfessionDistribution -> Either ValidationError StartingConfiguration
          internalMake _startingSize _goldPercent _silverPercent _ironPercent _professionDistribution
            | percentSum == Percent 1.0 = Right StartingConfiguration {startingSize = _startingSize, goldPercent = _goldPercent, silverPercent = _silverPercent, ironPercent = _ironPercent, ironProfessionDistribution = _professionDistribution}
            | otherwise = Left PercentagesMustAddToOne
              where percentSum = _goldPercent + _silverPercent + _ironPercent

generateStartingState :: StartingConfiguration -> State
generateStartingState startingConfiguration =
    State {
        citizens =
            generateGoldCitizens startingSize' (goldPercent startingConfiguration)
            ++ generateSilverCitizens startingSize' (silverPercent startingConfiguration)
            ++ generateIronCitizens startingConfiguration,
        time = 0
    }
    where startingSize' = startingSize startingConfiguration

generateCitizens :: (() -> Citizen) -> StartingSize -> Percent -> [Citizen]
generateCitizens initFunction (StartingSize startingSize') (Percent percent) = replicate numberOfCitizens (initFunction ())
    where numberOfCitizens = round (fromIntegral startingSize' * percent)

generateGoldCitizens :: StartingSize -> Percent -> [Citizen]
generateGoldCitizens  = generateCitizens (const Gold { age = 0 })

generateSilverCitizens :: StartingSize -> Percent -> [Citizen]
generateSilverCitizens  = generateCitizens (const Silver { age = 0 })

generateIronCitizens :: StartingConfiguration -> [Citizen]
generateIronCitizens startingConfiguration = concatMap (generateIronCitizensWithProfession (startingSize startingConfiguration) (ironPercent startingConfiguration)) professionPairs
    where professionPairs = Map.toList (getMap $ ironProfessionDistribution startingConfiguration)

generateIronCitizensWithProfession :: StartingSize -> Percent -> (Profession, Double) -> [Citizen]
generateIronCitizensWithProfession startingSize' (Percent ironPercent') (professionValue, professionProportion) = generateCitizens (const Iron { age = 0, profession = professionValue }) startingSize' (Percent (ironPercent' * professionProportion))
