import Control.Monad.Instances

-- mapping (*3) over (+100) i.e.
-- Create a function that applies (+100) to input, then apply (*3) to that.
-- apply that resultant function to 1
a = fmap (*3) (+100) 1
b = (*3) `fmap` (+100) $ 1
c = (*3) . (+100) $ 1
z = fmap (show . (*3)) (+100) 1
