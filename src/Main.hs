import System.IO
import qualified Data.Map as Map

data Profession = Farmer | Baker | Miner | Blacksmith deriving (Show, Ord, Eq, Bounded, Enum)
data Citizen = Gold { age :: Int } | Silver { age :: Int } | Iron { age :: Int, profession :: Profession } deriving Show
data State = State { citizens :: [Citizen], time :: Int } deriving Show
data StartingConfiguration = StartingConfiguration {startingSize :: Int, goldPercent :: Double, silverPercent :: Double, ironPercent :: Double, ironProfessionDistribution :: Map.Map Profession Double }

allProfessions :: [Profession]
allProfessions = [minBound .. maxBound]

myStartingConfiguration :: StartingConfiguration = StartingConfiguration {
    startingSize = 100,
    goldPercent = 0.1,
    silverPercent = 0.2,
    ironPercent = 0.7,
    ironProfessionDistribution = Map.fromList [(Farmer, 0.3), (Baker, 0.2), (Miner, 0.3), (Blacksmith, 0.2)] }
startingState :: State = generateStartingState myStartingConfiguration

generateStartingState :: StartingConfiguration -> State
generateStartingState startingConfiguration = State { citizens =  generateGoldCitizens startingConfiguration ++ generateSilverCitizens startingConfiguration ++ generateIronCitizens startingConfiguration, time = 0 }

generateCitizens :: StartingConfiguration -> Double -> (() -> Citizen) -> [Citizen]
generateCitizens startingConfiguration percent initFunction = replicate (round (fromIntegral (startingSize startingConfiguration) * percent)) (initFunction ())

generateGoldCitizens :: StartingConfiguration -> [Citizen]
generateGoldCitizens startingConfiguration = generateCitizens startingConfiguration (goldPercent startingConfiguration) (const Gold { age = 0})

generateSilverCitizens :: StartingConfiguration -> [Citizen]
generateSilverCitizens startingConfiguration = generateCitizens startingConfiguration (silverPercent startingConfiguration) (const Silver { age = 0})

unwrap :: Maybe a -> a
unwrap (Just a) = a
unwrap Nothing = error "Unwrapping none value"

generateIronCitizens :: StartingConfiguration -> [Citizen]
generateIronCitizens startingConfiguration = concatMap (generateIronCitizensWithProfession startingConfiguration) (generateProfessionDistribution startingConfiguration)

generateProfessionDistribution :: StartingConfiguration -> [(Profession, Double)]
generateProfessionDistribution startingConfiguration = map (\profession -> (profession, unwrap $ Map.lookup profession (ironProfessionDistribution startingConfiguration))) allProfessions

generateIronCitizensWithProfession :: StartingConfiguration -> (Profession, Double) -> [Citizen]
generateIronCitizensWithProfession startingConfiguration (profession, percent) = generateCitizens startingConfiguration (ironPercent startingConfiguration * percent) (const Iron {age = 0, profession = profession})

main = putStrLn "Hello World"
