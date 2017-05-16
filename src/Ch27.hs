{-# LANGUAGE BangPatterns #-}

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


--

-- 1. let x = 1
-- :sprint x
-- x = _

-- 2. let x = ['1']
-- :sprint x
-- x = "1"

-- 3. let x = [1]
-- :sprint x
-- x = _

-- let x = 1 :: Int
-- :sprint x
-- x = 1

-- let f = \x -> x
-- let x = f 1
-- :sprint x
-- x = _

-- let f :: Int -> Int; f = \x -> x
-- x = f 1
-- :sprint x
-- x = _

-- will printing this result in bottom?

-- 1. snd (undefined, 1)
-- no

-- 2. let x = undefined
-- let y = x `seq` 1 in snd (x, y)
-- yes

-- 3. length $ [1..5] ++ undefined
-- yes

-- 4. length $ [1..5] ++ [undefined]
-- no

-- const 1 undefined
-- no

-- const 1 (undefined `seq` 1)
-- no

-- const undefined 1
-- yes

-- Make the expr bottom

x = undefined
y = "blah"

maindo = do
    print (snd (!x, y))

