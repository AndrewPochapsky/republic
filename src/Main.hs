module Main where

import qualified Data.Map as Map
import           Init     (GoldPercent (..), IronPercent (..),
                           SilverPercent (..), StartingConfiguration,
                           StartingSize (..), generateStartingState,
                           mkStartingConfiguration)
import           Structs

myStartingConfiguration :: StartingConfiguration
startingState :: State

myStartingConfiguration = case configEither of
    Right config' -> config'
    Left e        -> error (show e)
    where configEither = mkStartingConfiguration (StartingSize 200) (GoldPercent 0.1) (SilverPercent 0.2) (IronPercent 0.7) (Map.fromList [(Farmer, 0.3), (Baker, 0.2), (Miner, 0.3), (Blacksmith, 0.2)])

startingState = generateStartingState myStartingConfiguration

advance :: State -> State
advance state = State {
    citizens = map
        (\citizen -> citizen { age = age citizen + 1 })
        (citizens state),
    time = time state + 1,
    professionProductionMap = professionProductionMap state,
    resources = resources state
    }

main :: IO()
main = print $ advance startingState
