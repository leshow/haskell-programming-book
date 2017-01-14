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
import           Data.List                (elemIndex)
import           Data.Monoid              ((<>))
import           Test.QuickCheck          hiding (Failure, Success)
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

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

data Validation err a
    = Failure err
    | Success a
    deriving (Eq, Show)

validToEither :: Validation e a -> Either e a
validToEither (Failure err) = Left err
validToEither (Success a)   = Right a

eitherToValid :: Either e a -> Validation e a
eitherToValid (Left err) = Failure err
eitherToValid (Right a)  = Success a

instance Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
    pure = Success
    (Failure e) <*> (Failure e')    = Failure $ e <> e'
    _ <*> (Failure e')              = Failure e'
    (Failure e) <*> _               = Failure e
    Success f <*> Success a         = Success $ f a

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = do
        a <- arbitrary
        e <- arbitrary
        elements [Success a, Failure e]

instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) = eq

testVal = do
  putStrLn "-- applicative Validation"
  quickBatch (applicative (undefined :: Validation S (S, S, S)))

-- chapter exercises

-- 1
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
    pure a = Pair a a
    Pair a a' <*> Pair b b' = Pair (a b) (a' b')

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        a' <- arbitrary
        return $ Pair a a'

instance Eq a => EqProp (Pair a) where
    (=-=) = eq

-- 2

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure = Two mempty
    Two a b <*> Two a' b' = Two (a <> a') (b b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

-- 3
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty
    Three a b c <*> Three a' b' c' = Three (a <> a') (b <> b') (c c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

-- 4

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
    pure b = Three' mempty b b
    Three' a b b' <*> Three' as bs bs' = Three' (a <> as) (b bs) (b' bs')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        b' <- arbitrary
        return $ Three' a b b'

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq


-- 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure = Four mempty mempty mempty
    Four a b c d <*> Four a' b' c' d' = Four (a <> a') (b <> b') (c <> c') (d d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq

-- 6

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance Monoid a => Applicative (Four' a) where
    pure = Four' mempty mempty mempty
    Four' a a' a'' b <*> Four' as as' as'' bs = Four' (a <> as) (a' <> as') (a'' <> as'') (b bs)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        a' <- arbitrary
        a'' <- arbitrary
        b <- arbitrary
        return $ Four' a a' a'' b

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

type S = String
type I = Int

runAppTests :: IO ()
runAppTests = do
    putStr "\n-- Pair"
    quickBatch (applicative (undefined :: Pair (I, I, I)))
    putStr "\n-- Two"
    quickBatch (applicative (undefined :: Two (S, S, S) (I, I, I)))
    putStr "\n-- Three"
    quickBatch (applicative (undefined :: Three (S, S, S) (S, S, S) (I, I, I)))
    putStr "\n-- Three'"
    quickBatch (applicative (undefined :: Three' (S, S, S) (I, I, I)))
    putStr "\n-- Four"
    quickBatch (applicative (undefined ::
                              Four (S, S, S)
                                   (S, S, S)
                                   (S, S, S)
                                   (I, I, I)))
    putStr "\n-- Four'"
    quickBatch (applicative (undefined :: Four' (S, S, S) (I, I, I)))


stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
