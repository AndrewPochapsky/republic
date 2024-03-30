module Init
    ( GoldPercent (..)
    , IronPercent (..)
    , SilverPercent (..)
    , StartingConfiguration
    , StartingSize (..)
    , generateStartingState
    , mkStartingConfiguration
    ) where

import qualified Data.Map        as Map
import           DistributionMap (DistributionMap, DistributionMapError,
                                  ExhaustiveDistributionMap, mkDistributionMap,
                                  mkExhaustiveDistributionMap)
import qualified DistributionMap (toList)
import           ExhaustiveMap   (ExhaustiveMapClass (lookup))
import           Numeric.Natural
import           Percent         (Percent (getValue))
import           Structs         (Citizen (..), CitizenType (..), Profession,
                                  State (..))

data ValidationError
  = InvalidProfessionDistribution
  | InvalidPercentage
  | PercentagesMustAddToOne
  | DistributionMapError DistributionMapError
  deriving (Show)

convertDistributionMapError :: Either DistributionMapError a -> Either ValidationError a
convertDistributionMapError eitherValue = case eitherValue of
    Left err      -> Left (DistributionMapError err)
    (Right value) -> Right value

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

type ProfessionDistribution = DistributionMap.DistributionMap Profession

data StartingConfiguration
  = StartingConfiguration
      { startingSize               :: StartingSize
      , populationDMap             :: ExhaustiveDistributionMap CitizenType
      , ironProfessionDistribution :: ProfessionDistribution
      }
  deriving (Show)

mkStartingConfiguration :: StartingSize -> GoldPercent -> SilverPercent -> IronPercent -> Map.Map Profession Double -> Either ValidationError StartingConfiguration

mkStartingConfiguration startingSize (GoldPercent goldPercent) (SilverPercent silverPercent) (IronPercent ironPercent) professionDistributionMap' =
     do
        populationDMap <- convertDistributionMapError $ mkExhaustiveDistributionMap $ Map.fromList [(GoldType, goldPercent), (SilverType, silverPercent), (IronType, ironPercent)]
        ironProfessionDistribution <- convertDistributionMapError $ mkDistributionMap professionDistributionMap'
        return $ StartingConfiguration {startingSize, populationDMap, ironProfessionDistribution}

generateStartingState :: StartingConfiguration -> State
generateStartingState startingConfiguration =
    State {
        citizens =
            generateGoldCitizens startingSize' (ExhaustiveMap.lookup GoldType (populationDMap startingConfiguration))
             ++ generateSilverCitizens startingSize' (ExhaustiveMap.lookup SilverType (populationDMap startingConfiguration))
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
    concatMap (generateIronCitizensWithProfession (startingSize startingConfiguration) (ExhaustiveMap.lookup IronType (populationDMap startingConfiguration))) professionPairs
    where professionPairs = DistributionMap.toList $ ironProfessionDistribution startingConfiguration

generateIronCitizensWithProfession :: StartingSize -> Percent -> (Profession, Percent) -> [Citizen]
generateIronCitizensWithProfession startingSize' ironPercent' (profession, professionPercent) =
    generateCitizens Iron { age = 0, profession } startingSize' (ironPercent' * professionPercent)
