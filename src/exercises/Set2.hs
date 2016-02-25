{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module MonadChallenges.Set2
    (

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

liftPair :: Applicative f => f a -> f b -> f (a, b)
liftPair = liftA2 (,)

liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = fmap f a <*> b 

mapTuple :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapTuple f g t = (f $ fst t, g $ snd t)

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    show Nothing  = "Nothing"
    show (Just val) = "Just " ++ show val

instance Functor Maybe where
    fmap f (Just a) = Just (f a)
    fmap f Nothing = Nothing

mkMaybe :: a -> Maybe a
mkMaybe = Just

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = mkMaybe x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = mkMaybe xs

lookupMay :: Eq a => a -> [(a,b)] -> Maybe b
lookupMay key [] = Nothing
lookupMay key ((x,y):xs)
    | key == x   = mkMaybe y
    | otherwise  = lookupMay key xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay x 0 = Nothing
divMay x y = mkMaybe (x / y)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay as = Just $ foldl1 step as
    where
        step :: Ord a => a -> a -> a
        step x y
            | x <= y    = y 
            | otherwise = x

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay as = Just $ foldl1 step as
    where
        step :: Ord a => a -> a -> a
        step x y
            | x <= y    = x
            | otherwise = y

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd s = 
    case lookupMay s gd of
        Nothing -> Nothing
        Just xs -> 
            case tailMay xs of
                Nothing -> Nothing
                Just ys -> 
                    case maximumMay ys of
                        Nothing -> Nothing
                        Just y  ->
                            case headMay xs of
                                Nothing -> Nothing
                                Just x  -> divMay (fromIntegral y) (fromIntegral x)



chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f a = case a of
    Nothing -> Nothing
    Just x  -> f x

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain


queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gd str = 
    lookupMay str gd `link` \xs ->
    combine (headMay xs) (tailMay xs `link` maximumMay) `link`
    (uncurry divMay . toDouble)
    where 
        combine (Just x) (Just y) = Just (x, y)
        combine _ _               = Nothing
        toDouble (x, y)           = (fromIntegral x, fromIntegral y)


yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f (Just m1) (Just m2) = mkMaybe (f m1 m2)
yLink _ _ _                 = Nothing

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries salaries str1 str2 = yLink (+) (lookupMay str1 salaries) (lookupMay str2 salaries)

tailProd :: Num a => [a] -> Maybe a
tailProd = fmap product . tailMay 

tailSum :: Num a => [a] -> Maybe a
tailSum  = fmap sum . tailMay


tailMax :: Ord a => [a] -> Maybe a
tailMax = combine . fmap maximumMay . tailMay

tailMin :: Ord a => [a] -> Maybe a
tailMin = combine . fmap minimumMay . tailMay

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing   = Nothing
combine (Just ma) = ma