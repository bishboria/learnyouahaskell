type Birds = Int
type Pole  = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

walked = return (0,0) >>= landLeft 2 >>= landRight 1 >>= landLeft 2 >>= landRight (-1)
-- walked == Nothing

banana  :: Pole -> Maybe Pole
banana _ = Nothing

slip = return (0,0) >>= landLeft 1 >>= banana >>= landRight 1
-- slip == Nothing

-- What would this look like if we hadn't treated Maybe values as values
-- with a failure context?
routine :: Maybe Pole
routine = case landLeft 1 (0,0) of
    Nothing    -> Nothing
    Just pole1 -> case landRight 4 pole1 of
        Nothing    -> Nothing
        Just pole2 -> case landLeft 2 pole2 of
            Nothing    -> Nothing
            Just pole3 -> landLeft 1 pole3
