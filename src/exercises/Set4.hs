{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude

infixl 4 <*>, <*, *>, <$>
infixl 1 >>=, =<<, >>

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

class Applicative m => Monad m where
    return :: a -> m a

    (>>=) :: m a -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b
    ma >> mb = ma >>= \_ -> mb


-- | Infix form of fmap
(<$>) :: Functor f => (a -> b) -> f a -> f b
f <$> a = fmap f a

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

mapTuple :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapTuple f g t = (f $ fst t, g $ snd t)

liftPair :: Applicative f => f a -> f b -> f (a, b)
liftPair = liftA2 (,)

liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = fmap f a <*> b 

liftM :: Monad m => (a1 -> r) -> m a1 -> m r
liftM = liftA

liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 = liftA2

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

sequence :: Monad m => [m a] -> m [a]
sequence ms = foldl (liftA2 (flip (:))) (return []) ms

join :: Monad m => m (m a) -> m a
join ma = ma >>= id

-- | We can generalize this.
-- | nRands :: Gen a -> Integer -> Gen [a]
nRands :: Gen a -> Integer -> Gen [a]
nRands gen n = sequence $ take (fromIntegral n) (iterate step gen)
    where
        step :: Gen a -> Gen a
        step = liftA id

randIntegers :: Integer -> Gen [Integer]
randIntegers n = nRands randInteger n

-- | Use a ByteString here instead of a String
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
randString n = nRands randLetter n

fiveRands :: Gen [Integer]
fiveRands = randIntegers 5

randEven  :: Gen Integer
randEven  = fmap (\x -> 2*x) randInteger

randOdd   :: Gen Integer
randOdd   = fmap (\x -> 2*x+1) randInteger

randTen   :: Gen Integer
randTen   = fmap (\x -> 10*x) randInteger

randPair  :: Gen (Char, Integer)
randPair  = liftPair randLetter randInteger 


-- | The Maybe monad code

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    show Nothing  = "Nothing"
    show (Just val) = "Just " ++ show val

instance Functor Maybe where
    fmap f (Just a) = Just (f a)
    fmap f Nothing = Nothing

instance Applicative Maybe where
    pure = Just

    Just f <*> m  = fmap f m
    Nothing <*> _ = Nothing

instance Monad Maybe where
    return = pure

    (Just a) >>= f = f a
    Nothing  >>= _ = Nothing


headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = return x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = return xs

lookupMay :: Eq a => a -> [(a,b)] -> Maybe b
lookupMay _key [] = Nothing
lookupMay key ((x,y):xs)
    | key == x   = return y
    | otherwise  = lookupMay key xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay x 0 = Nothing
divMay x y = return (x / y)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay as = return $ foldl1 step as
    where
        step :: Ord a => a -> a -> a
        step x y
            | x <= y    = y 
            | otherwise = x

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay as = return $ foldl1 step as
    where
        step :: Ord a => a -> a -> a
        step x y
            | x <= y    = x
            | otherwise = y

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gd str = 
    lookupMay str gd >>= \xs ->
    combine (headMay xs) (tailMay xs >>= maximumMay) >>=
    (uncurry divMay . toDouble)
    where 
        combine (Just x) (Just y) = Just (x, y)
        combine _x _y             = Nothing
        toDouble (x, y)           = (fromIntegral x, fromIntegral y)

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries salaries str1 str2 = liftM2 (+) (lookupMay str1 salaries) (lookupMay str2 salaries)

tailProd :: Num a => [a] -> Maybe a
tailProd = fmap product . tailMay 

tailSum :: Num a => [a] -> Maybe a
tailSum  = fmap sum . tailMay

tailMax :: Ord a => [a] -> Maybe a
tailMax = join . fmap maximumMay . tailMay

tailMin :: Ord a => [a] -> Maybe a
tailMin = join . fmap minimumMay . tailMay