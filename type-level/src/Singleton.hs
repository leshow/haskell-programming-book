{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Singleton where

import           Data.Singletons
import           Data.Singletons.TH
import           Data.Kind

$(singletons [d|
    data Nat = Z | S Nat
        deriving (Show, Eq, Ord)
 |])


