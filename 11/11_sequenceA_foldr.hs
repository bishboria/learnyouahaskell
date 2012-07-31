import Control.Applicative

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

a = sequenceA [Just 1, Just 2]
