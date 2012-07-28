-- Functors Redux
--
-- functors are things that can be mapped over like lists, Maybes and trees
-- They are described by the type class Functor which has one type class method
-- fmap
--
-- :t fmap
-- fmap :: (a -> b) -> f a -> f b
--
-- to make a type constructor an instance of Functor, its kind must be
-- * -> *
-- e.g. Maybe
--
-- To make Either into a Functor instance have to write:
--
-- instace Functor (Either a) where
-- 
-- its type would then be:
-- :t fmap
-- fmap :: (b -> c) -> Either a b -> Either a c


-- IO Actions as Functors
--
-- IO is an instance of Functor
--
-- instance Functor IO where
--     fmap f action = do
--         result <- action
--         return (f result)

-- E.g.
main = do line <- getLine
          let line' = reverse line
          putStrLn $ "You said " ++ line' ++ " backwards!"
          putStrLn $ "Yes, you said " ++ line' ++ " backwards!"
-- run ./11_backwards_1

-- Rewritten using fmap
main = do line <- fmap reverse getLine
          putStrLn $ "You said " ++ line ++ " backwards!"
          putStrLn $ "Yes, you really said " ++ line ++ " backwards!"
-- run ./11_backwards_2

-- mapping reverse over getLine gives us an IO Action that gets a line from
-- standad input and applies reverse to its result
-- biding the result to a name using <- gives a value that has already had
-- reverse applied to it.
--
-- If you ever find yourself binding the respult of an IO action to a name,
-- only then to apply a function to that and call it something else, that's
-- a good candidate for using fmap
--
-- Example of applying more than one function to data inside a functor
import Data.Char
import Data.List

main = do
    line <- fmap (intersperse '-' . reverse . map toUpper) getLine
    putStrLn line
-- run ./11_reverse_intersperse
