{-# LANGUAGE ApplicativeDo #-}

module Ch17 where

-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- class Functor f => Applicative f where
--      pure :: a -> f a
--      (<*>) :: f (a -> b) -> f a -> f b
-- every applicative must also have a functor instance
-- Control.Applicative also provides liftA liftA2 liftA3
-- liftA is fmap but with a applicative constraint instead of functor
-- liftA2 and A3 are fmap but can take more arguments
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c

-- Applicatives are Monoidal Functors

-- also, you can define functor in terms of fmap, for example
-- fmap f x == pure f <*> x
-- and
-- fmap (+1) [1,2,3] == pure (+1) <*> [1,2,3]


-- ("Woo", (+1)) <*> ("Hoo!", 0) -- ("WooHoo!", 1)

-- notice the strings concatenated. this is due to Monoid instances for that value

import           Control.Applicative
import           Data.List           (elemIndex)
import           Data.Monoid         ((<>))

added :: Maybe Integer
added = (+3) <$> lookup 3 (zip [1,2,3] [4,5,6])

y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]


tupled :: Maybe (Integer, Integer)
tupled = liftA2 (,) y z
-- (,) <$> y <*> zpp

x' :: Maybe Int
x' = elemIndex 3 [1..5]

y' :: Maybe Int
y' = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'
-- :t max' <$> x'
-- Maybe (Int -> Int)  aka. f (a -> b) -- the first arg to applicative's <*>
-- so we can take that and use <*> y' to pass in f a

xs = [1, 2, 3]
ys = [4, 5, 6]

x'' :: Maybe Integer
x'' = lookup 2 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x'' <*> y'')
-- sum <$> liftA2 (,) x'' y''


newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) fa = f <$> fa

newtype Constant a b
    = Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure a = Constant { getConstant = mempty }
    -- following are both valid (I think)
    -- (<*>) (Constant a) (Constant b) = Constant { getConstant = a `mappend` b }
    fab <*> fa = Constant (getConstant fab <> getConstant fa)

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
    if length s > maxLen
        then Nothing
        else Just s

newtype Name = Name String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s
-- mkName s = Name <$> validateLength 25 s
-- mkName = (Name <$>) . (validateLength 25)
-- mkName = (Name <$>) . validateLength 25

mkAddress :: String -> Maybe Address
mkAddress a = Address <$> validateLength 100 a

data Person
    = Person Name Address
    deriving (Eq, Show)

-- mkPerson :: String -> String -> Maybe Person
-- mkPerson n a =
--     case mkName n of
--         Just name ->
--             case mkAddress a of
--                 Just add -> Just $ Person name add
--                 Nothing  -> Nothing
--         Nothing -> Nothing

-- λ> let name = mkName "Dildo Baggins"
-- λ> :t name
-- name :: Maybe Name
-- λ> let address = mkAddress "the shire"
-- λ> :t address
-- address :: Maybe Address
-- λ> :t Person <$> name
-- Person <$> name :: Maybe (Address -> Person)
-- λ> :t Person <$> name <*> address
-- Person <$> name <*> address :: Maybe Person

mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person <$> mkName n <*> mkAddress a

-- attempt with ApplicativeDo
mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = do
    x <- mkName n
    y <- mkAddress a
    return $ Person x y


data Cow
    = Cow {
    name     :: String
    , age    :: Int
    , weight :: Int
    } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n    | n >= 0 = Just n
                | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString n a w =
    Cow <$> noEmpty n
    <*> noNegative a
    <*> noNegative w
-- liftA3 Cow (noEmpty n) (noNegative a) (noNegative w)

-- Exercises
ax = const <$> Just "Hello" <*> pure "World"
bx = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3]

-- Applicative Laws
-- Identity:
-- pure id <*> v = v

-- A homomorphism is a structure preserving map between two algebraic structures
-- pure f <*> pure x = pure (f x)


data List a
    = Nil
    | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons a la) = Cons (f a) (fmap f la)

instance Monoid (List a) where
    mempty = Nil
    mappend Nil lb         = lb
    mappend (Cons a la) lb = Cons a (la `mappend` lb)

instance Applicative List where
    pure a = Cons a Nil

    (<*>) Nil Nil = Nil
    (<*>) Nil _   = Nil
    (<*>) _ Nil   = Nil
    --(Cons f fs) <*> (Cons a as) = Cons (f a) (fs <*> as) -- type checks but is wrong
    (Cons f fs) <*> as = mappend (fmap f as) (fs <*> as)
    --fold (<>) Nil (fmap f <$> Cons lb Nil) <> (fa <*> lb) -- also seems to be correct but is long


append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap _ Nil = Nil
flatMap f as  = concat' (fmap f as)
