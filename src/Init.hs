{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Data.Bifunctor  (second)
import qualified Data.Map        as Map
import           Numeric.Natural
import           Percent         (Percent (getValue), PercentValidationError,
                                  mkPercentFrom, oneHundredPercent)
import           Structs         (Citizen (..), Profession, State (..))
import           Utils           (invertEitherList)

data ValidationError
  = InvalidProfessionDistribution
  | InvalidPercentage
  | PercentagesMustAddToOne
  | PercentValidationError PercentValidationError
  deriving (Show)

convertPercentError :: Either PercentValidationError a -> Either ValidationError a
convertPercentError eitherValue = case eitherValue of
    Left err      -> Left (PercentValidationError err)
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

newtype ProfessionDistribution
  = ProfessionDistribution { getMap :: Map.Map Profession Percent }
  deriving (Show)

mkProfessionDistribution :: Map.Map Profession Double -> Either ValidationError ProfessionDistribution
mkProfessionDistribution rawMap = join $ do
    parsedList :: [(Profession, Percent)] <- convertPercentError $ invertEitherList (map (second $ mkPercentFrom id) (Map.toList rawMap))
    return $ case sumOfFractions of
        1.0 -> Right $ ProfessionDistribution (Map.fromList parsedList)
        _   -> Left InvalidProfessionDistribution
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

mkStartingConfiguration :: StartingSize -> GoldPercent -> SilverPercent -> IronPercent -> Map.Map Profession Double -> Either ValidationError StartingConfiguration

mkStartingConfiguration startingSize' goldPercent' silverPercent' ironPercent' professionDistributionMap' =
     join $ do
        gold <- convertPercentError $ mkPercentFrom getGoldPercent goldPercent'
        silver <- convertPercentError $ mkPercentFrom getSilverPercent silverPercent'
        iron <- convertPercentError $ mkPercentFrom getIronPercent ironPercent'
        professionDistribution <- mkProfessionDistribution professionDistributionMap'
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

generateCitizens :: (() -> Citizen) -> StartingSize -> Percent -> [Citizen]
generateCitizens initFunction (StartingSize startingSize') percent = replicate numberOfCitizens (initFunction ())
    where numberOfCitizens = round (fromIntegral startingSize' * getValue percent)

generateGoldCitizens :: StartingSize -> Percent -> [Citizen]
generateGoldCitizens  = generateCitizens (const Gold { age = 0 })

generateSilverCitizens :: StartingSize -> Percent -> [Citizen]
generateSilverCitizens  = generateCitizens (const Silver { age = 0 })

generateIronCitizens :: StartingConfiguration -> [Citizen]
generateIronCitizens startingConfiguration =
    concatMap (generateIronCitizensWithProfession (startingSize startingConfiguration) (ironPercent startingConfiguration)) professionPairs
    where professionPairs = Map.toList (getMap $ ironProfessionDistribution startingConfiguration)

generateIronCitizensWithProfession :: StartingSize -> Percent -> (Profession, Percent) -> [Citizen]
generateIronCitizensWithProfession startingSize' ironPercent' (professionValue, professionProportion) =
    generateCitizens (const Iron { age = 0, profession = professionValue }) startingSize' (ironPercent' * professionProportion)
