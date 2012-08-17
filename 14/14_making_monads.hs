import Data.Ratio
import Control.Monad

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving (Show)

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x, p)) xs

flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs, p) = map (\(x, r) -> (x, p*r)) innerxs

instance Monad Prob where
    return x = Prob [(x,1%1)]
    m >>= f  = flatten (fmap f m)
    fail _   = Prob []

situation :: Prob (Prob Char)
situation = Prob
    [(Prob [('a',1%2),('b',1%2)], 1%4)
    ,(Prob [('c',1%2),('d',1%2)], 3%4)
    ]
