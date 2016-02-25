{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module MonadChallenges.Set1
    (
        Functor(..),
        Applicative(..),
        Monad(..),
        liftPair,
        liftA,
        liftA2,
        Gen(..),
        sequenceGen,
        repRandom,
        nRands,
        randInteger,
        randLetter,
        randString,
        fiveRands,
        randEven,
        randOdd,
        randTen,
        randPair,
    )
    where

import MCPrelude

infixl 4 <*>, <*, *>, <$>

class Functor f where
    fmap :: (a -> b) -> f a -> f b

    (<$) :: a -> f b -> f a
    (<$) = fmap . const

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    
    (*>) :: f a -> f b -> f b
    a1 *> a2 = (id <$ a1) <*> a2

    (<*) :: f a -> f b -> f a
    (<*) = liftA2 const

class Monad m where
    return :: a -> m a

    (>>=) :: m a -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b
    ma >> mb = ma >>= \_ -> mb


-- | Infix form of fmap
(<$>) :: Functor f => (a -> b) -> f a -> f b
f <$> a = fmap f a

mapTuple :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapTuple f g t = (f $ fst t, g $ snd t)

liftPair :: Applicative f => f a -> f b -> f (a, b)
liftPair = liftA2 (,)

liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = fmap f a <*> b 

newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

instance Functor Gen where
    fmap f gen = Gen $ \seed -> mapTuple f id $ runGen gen seed

instance Applicative Gen where
    pure a = Gen $ \seed -> (a, seed)

    f <*> gen = Gen $ \seed -> 
        let (a, seed') = runGen gen seed
            (f', seed'') = runGen f seed' 
        in  (f' a, seed'')

instance Monad Gen where
    return = pure

    ga >>= fb = Gen $ \seed -> let (a, seed') = runGen ga seed in runGen (fb a) seed'


-- | This is actually quite general. The Monad property of Gen gives us 
-- | sequence for free if we were using Control.Monad.
sequenceGen :: [Gen a] -> Gen [a]
sequenceGen gens = foldl step (pure []) gens
    where
        step :: Gen [a] -> Gen a -> Gen [a]
        step = liftA2 (flip (:))

repRandom :: [Gen a] -> Gen [a]
repRandom = sequenceGen

-- | We can generalize this.
-- | nRands :: Gen a -> Integer -> Gen [a]
nRands :: Integer -> Gen [Integer]
nRands n = sequenceGen $ take (fromIntegral n) (iterate step randInteger)
    where
        step :: Gen Integer -> Gen Integer
        step = liftA id

-- | Use a ByteString here instead of a String?
letters :: String
letters = "abcdefghijklmnopqrstuvwxyz"

letterCount :: Integer
letterCount = toInteger (length letters)

randInteger :: Gen Integer
randInteger = Gen rand

randLetter :: Gen Char
randLetter = fmap getLetter randInteger
    where
        getLetter i = letters !! fromIntegral (i `rem` letterCount)

randString :: Integer -> Gen String
randString n = sequenceGen $ take (fromIntegral n) (iterate step randLetter)
    where
        step :: Gen Char -> Gen Char
        step = liftA id

fiveRands :: Gen [Integer]
fiveRands = nRands 5

randEven  :: Gen Integer
randEven  = fmap (\x -> 2*x) randInteger

randOdd   :: Gen Integer
randOdd   = fmap (\x -> 2*x+1) randInteger

randTen   :: Gen Integer
randTen   = fmap (\x -> 10*x) randInteger

randPair  :: Gen (Char, Integer)
randPair  = liftPair randLetter randInteger 
