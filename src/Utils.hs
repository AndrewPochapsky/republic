module Utils
    ( expectJust
    , expectRight
    , invertEitherList
    ) where

invertEitherList :: [(k, Either e v)] -> Either e [(k, v)]
invertEitherList [] = Right []
invertEitherList ((k, eitherVal):rest) = do
    value <- eitherVal
    rest' <- invertEitherList rest
    return ((k, value) : rest')

expectJust :: Maybe a -> a
expectJust (Just value) = value
expectJust Nothing      = error ""

expectRight :: (Show err) => Either err value -> value
expectRight (Left e)  = error (show e)
expectRight (Right v) = v
