module Ch24 where


-- Learning Parsers

import           Text.Parser.Combinators
import           Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1' -- read a single char '1'
one' = one >> stop -- read a sincle char '1' then die

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

testeof :: Parser () -> IO ()
testeof p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

run = do
    pNL "stop:"
    testParse stop
    pNL "one:"
    testParse one
    pNL "one':"
    testParse one'
    pNL "oneTwo:"
    testParse oneTwo
    pNL "oneTwo':"
    testParse oneTwo'
    pNL "eof one"
    testeof (one >> eof)
    pNL "eof oneTwo"
    testeof (oneTwo >> eof)

one1 :: CharParsing m => m String
one1 = string "1"

one2 :: CharParsing m => m String
one2 = string "12"

one3 :: CharParsing m => m String
one3 = string "123"
