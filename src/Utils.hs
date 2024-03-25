module Utils
    ( invertEitherList
    ) where

invertEitherList :: [(k, Either e v)] -> Either e [(k, v)]
invertEitherList [] = Right []
invertEitherList ((k, eitherVal):rest) = do
    value <- eitherVal
    rest' <- invertEitherList rest
    return ((k, value) : rest')
