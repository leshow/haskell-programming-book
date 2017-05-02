-- exercises: evaluate

-- 1.
-- const 1 undefined
-- 1

-- 2.
-- const undefined 1
-- exception

-- 3.
-- flip const undefined 1
-- 1

-- 4.
-- flip const 1 undefined
-- exception

-- 5.
-- const undefined undefined
-- exception

-- 6.
-- foldr const 'z' ['a'..'e']
-- 'a'

-- 7.
-- foldr (flip const) 'z' ['a'..'e']
-- 'z'

import           Debug.Trace (trace)

inc = (+1)

twice = inc . inc

howManyTimes = inc (trace "eval'd" (1+1)) + twice (trace "eval'd" (1+1))

howManyTimes' = let onePlus = trace "eval'd" (1+1)
                in inc onePlus + twice onePlus


