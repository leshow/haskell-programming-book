module Ch24 where


-- Learning Parsers

import           Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1' -- read a single char '1'
one' = one >> stop -- read a sincle char '1' then die
