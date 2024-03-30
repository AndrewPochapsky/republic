module IntoMap
    ( IntoMap (intoMap)
    ) where

import qualified Data.Map as Map

class IntoMap m k v
    where intoMap :: m -> Map.Map k v

instance IntoMap (Map.Map k v) k v
    where intoMap = id

instance (Ord k) => IntoMap [(k, v)] k v
    where intoMap = Map.fromList
