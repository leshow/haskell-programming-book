module Ch26Ex where


import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Functor.Identity
import           Data.Monoid                ((<>))

rDec :: Num a => Reader a a
rDec = ReaderT $ \r -> return (r - 1)

rDec' :: Num a => Reader a a
rDec' = reader $ \r -> r - 1


rShow :: Show a => ReaderT a Identity String
rShow = reader $ \r -> show r
-- ReaderT $ \s -> return (show s)
-- ReaderT $ \s -> Identity (show s)


rShow' :: Show a => ReaderT a Identity String
rShow' = reader show


rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
                    liftIO $ putStrLn ("Hi: " <> show r) -- seems to work without liftIO
                    return (r+1)


sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
            liftIO $ putStrLn ("Hi: " <> show s)
            return (show s, s + 1)


isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
    v <- liftIO getLine
    guard $ isValid v
    return v

doExcite :: IO ()
doExcite = do
    putStrLn "say something excite!"
    excite <- runMaybeT maybeExcite
    case excite of
        Nothing -> putStrLn "MOAR EXCITE"
        Just e  -> putStrLn ("Good, was very excite: " ++ e)


