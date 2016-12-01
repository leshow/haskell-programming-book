module Ch15 where

import           Data.Monoid ((<>))

data Optional a
    = Nada
    | Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty     = Nada

    mappend Nada Nada         = Nada
    mappend Nada a            = a
    mappend a Nada            = a
    mappend (Only a) (Only b) = Only (a <> b)
