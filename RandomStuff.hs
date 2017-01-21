-- | I really don't know where to put this function. Why isn't it in @base@?
module RandomStuff where

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc [x] = Just ([], x)
unsnoc (x : xs) = (\(init, tail) -> (x : init, tail)) <$> unsnoc xs
