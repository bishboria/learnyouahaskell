data CMaybe a = CNothing | CJust Int a
    deriving (Show, Eq) -- C means counter

instance Functor CMaybe where
    fmap f CNothing          = CNothing
    fmap f (CJust counter x) = CJust (counter + 1) (f x)
-- Like the definition for Maybe except the additional increment of counter

-- :t CNothing
-- CNothing :: CMaybe a

-- :t CJust 0 "haha"
-- CJust 0 "haha" :: CMaybe [Char]

-- :t CJust 100 [1,2,3]
-- CJust 100 [1,2,3] :: Num t => CMaybe [t]

a = fmap (++"ha") (CJust 0 "ho")
-- CJust 1 "hoha"

b = fmap (++"he") (fmap (++"ha") (CJust 0 "ho"))
-- CJust 2 "hohahe"

c = fmap (++"blah") CNothing
-- CNothing

-- Does this obey functor laws? We only need one counter example

d = fmap id (CJust 0 "haha")
-- CJust 1 "haha"

e = id (CJust 0 "haha")
-- CJust 0 "haha"

f = d == e
-- False
