--import Structs (StartingConfiguration(StartingConfiguration), Profession)
module Main where

import System.IO
import qualified Data.Map as Map
import Structs

allProfessions :: [Profession]
allProfessions = [minBound .. maxBound]

myStartingConfiguration :: StartingConfiguration = StartingConfiguration {
    startingSize = 100,
    goldPercent = 0.1,
    silverPercent = 0.2,
    ironPercent = 0.7,
    ironProfessionDistribution = ProfessionDistribution (Map.fromList [(Farmer, 0.3), (Baker, 0.2), (Miner, 0.3), (Blacksmith, 0.2)]) }
startingState :: State = generateStartingState myStartingConfiguration

generateStartingState :: StartingConfiguration -> State
generateStartingState startingConfiguration = State { citizens =  generateGoldCitizens startingConfiguration ++ generateSilverCitizens startingConfiguration ++ generateIronCitizens startingConfiguration, time = 0 }

generateCitizens :: StartingConfiguration -> Double -> (() -> Citizen) -> [Citizen]
generateCitizens startingConfiguration percent initFunction = replicate (round (fromIntegral (startingSize startingConfiguration) * percent)) (initFunction ())

generateGoldCitizens :: StartingConfiguration -> [Citizen]
generateGoldCitizens startingConfiguration = generateCitizens startingConfiguration (goldPercent startingConfiguration) (const Gold { age = 0})

generateSilverCitizens :: StartingConfiguration -> [Citizen]
generateSilverCitizens startingConfiguration = generateCitizens startingConfiguration (silverPercent startingConfiguration) (const Silver { age = 0})

generateIronCitizens :: StartingConfiguration -> [Citizen]
generateIronCitizens startingConfiguration = concatMap (generateIronCitizensWithProfession startingConfiguration) (Map.toList (getMap $ ironProfessionDistribution startingConfiguration))

generateIronCitizensWithProfession :: StartingConfiguration -> (Profession, Double) -> [Citizen]
generateIronCitizensWithProfession startingConfiguration (profession, percent) = generateCitizens startingConfiguration (ironPercent startingConfiguration * percent) (const Iron {age = 0, profession = profession})

advance :: State -> State
advance state = State { citizens = map
    (\citizen -> citizen { age = age citizen + 1 })
    (citizens state), time = time state + 1}

main = putStrLn "Hello World"
