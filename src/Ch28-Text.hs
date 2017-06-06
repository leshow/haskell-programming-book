module Main where

import           Control.Monad
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified System.IO         as SIO

dictWordsTL :: IO TL.Text
dictWordsTL = TLIO.readFile "/usr/share/dict/words"

dictWords :: IO String
dictWords = SIO.readFile "/usr/share/dict/words"

dictWordsT :: IO T.Text
dictWordsT = TIO.readFile "/usr/share/dict/words"

main :: IO ()
main = do
    replicateM_ 10 (dictWords >>= print)
    replicateM_ 10 (dictWordsT >>= TIO.putStrLn)
    replicateM_ 10 (dictWordsTL >>= TLIO.putStrLn)
