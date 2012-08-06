-- MONOIDS


-- This type class if for types whose values can be combineg together with
-- a binary operation.


-- Wrapping an Existing Type into a New Type

-- In chapter 11, we saw a couple of ways for the list type to be an
-- applicative functor. One way is using <*>:
[(+1),(*100),(*5)] <*> [1,2,3]
-- [2,3,4,100,200,400,5,10,15]

-- The second way was using ZipLists:
getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3]
-- [2,200,15]

-- Because [] was already an instance of Applicative, we could declare
-- ZipList as:
data ZipList a = ZipList [a]
-- it only has one constructor and it has just one field. Using record
-- syntax:
data ZipList a = ZipList { getZipList :: [a] }

-- In the libraries ZipList is defined like:
newtype ZipList a = ZipList { getZipList :: [a] }

-- Why newtype?
--
-- It was made exactly for the cases when you want to wrap a type to
-- present it as a different type.
--
-- newtype is faster... There is some overhead using data to wrap/unwrap
-- values when the program is running. Using newtype, Haskell knows you're
-- just wrapping an existing type (same internally, but different type). It
-- then can get rid of all the wrapping/unwrapping
--
-- newtype can only have one value constructor and that constructor can
-- only have one field.
--
-- newtype declarations can also derive from other type classes. But the
-- type you're wrapping must already be an instance of that class!

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

CharList "this will be shown!"
-- CharList {getCharList = "this will be shown!"}
CharList "benny" == CharList "benny"
-- True
CharList "benny" == CharList "oysters"
-- False

-- In this particular newtype, the value constructor has the following type
CharList :: [Char] -> CharList
-- getCharList has type
getCharList :: CharList -> [Char]
-- this could be thought of as wrapping/unwrapping or as converting values
-- between types
