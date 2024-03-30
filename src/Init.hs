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
import           DistributionMap (DistributionMap, DistributionMapError,
                                  mkDistributionMap)
import qualified DistributionMap (toList)
import           Numeric.Natural
import           Percent         (Percent (getValue), PercentValidationError,
                                  mkPercent, oneHundredPercent)
import           Structs         (Citizen (..), Profession, State (..))

data ValidationError
  = InvalidProfessionDistribution
  | InvalidPercentage
  | PercentagesMustAddToOne
  | PercentValidationError PercentValidationError
  | DistributionMapError DistributionMapError
  deriving (Show)

convertPercentError :: Either PercentValidationError a -> Either ValidationError a
convertPercentError eitherValue = case eitherValue of
    Left err      -> Left (PercentValidationError err)
    (Right value) -> Right value

convertDistributionMapError :: Either DistributionMapError a -> Either ValidationError a
convertDistributionMapError eitherValue = case eitherValue of
    Left err      -> Left (DistributionMapError err)
    (Right value) -> Right value

newtype StartingSize
  = StartingSize Natural
  deriving (Show)

newtype GoldPercent
  = GoldPercent { getGoldPercent :: Double }
  deriving (Show)

newtype SilverPercent
  = SilverPercent { getSilverPercent :: Double }
  deriving (Show)

newtype IronPercent
  = IronPercent { getIronPercent :: Double }
  deriving (Show)

type ProfessionDistribution = DistributionMap.DistributionMap Profession

data StartingConfiguration
  = StartingConfiguration
      { startingSize               :: StartingSize
      , goldPercent                :: Percent
      , silverPercent              :: Percent
      , ironPercent                :: Percent
      , ironProfessionDistribution :: ProfessionDistribution
      }
  deriving (Show)

mkStartingConfiguration :: StartingSize -> GoldPercent -> SilverPercent -> IronPercent -> Map.Map Profession Double -> Either ValidationError StartingConfiguration

mkStartingConfiguration startingSize' goldPercent' silverPercent' ironPercent' professionDistributionMap' =
     join $ do
        gold <- convertPercentError $ mkPercent (getGoldPercent goldPercent')
        silver <- convertPercentError $ mkPercent (getSilverPercent silverPercent')
        iron <- convertPercentError $ mkPercent (getIronPercent ironPercent')
        professionDistribution <- convertDistributionMapError $ mkDistributionMap professionDistributionMap'
        return $ internalMake startingSize' gold silver iron professionDistribution
    where internalMake :: StartingSize -> Percent -> Percent -> Percent -> ProfessionDistribution -> Either ValidationError StartingConfiguration
          internalMake startingSize goldPercent silverPercent ironPercent ironProfessionDistribution
            | percentSum == oneHundredPercent = Right StartingConfiguration { startingSize, goldPercent, silverPercent, ironPercent, ironProfessionDistribution }
            | otherwise = Left PercentagesMustAddToOne
              where percentSum = goldPercent + silverPercent + ironPercent

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

generateCitizens :: Citizen -> StartingSize -> Percent -> [Citizen]
generateCitizens citizen (StartingSize startingSize') percent = replicate numberOfCitizens citizen
    where numberOfCitizens = round (fromIntegral startingSize' * getValue percent)

generateGoldCitizens :: StartingSize -> Percent -> [Citizen]
generateGoldCitizens  = generateCitizens Gold { age = 0 }

generateSilverCitizens :: StartingSize -> Percent -> [Citizen]
generateSilverCitizens  = generateCitizens Silver { age = 0 }

generateIronCitizens :: StartingConfiguration -> [Citizen]
generateIronCitizens startingConfiguration =
    concatMap (generateIronCitizensWithProfession (startingSize startingConfiguration) (ironPercent startingConfiguration)) professionPairs
    where professionPairs = DistributionMap.toList $ ironProfessionDistribution startingConfiguration

generateIronCitizensWithProfession :: StartingSize -> Percent -> (Profession, Percent) -> [Citizen]
generateIronCitizensWithProfession startingSize' ironPercent' (profession, professionPercent) =
    generateCitizens Iron { age = 0, profession } startingSize' (ironPercent' * professionPercent)
