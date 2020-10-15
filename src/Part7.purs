module Part7 where

import Prelude

{-
class Functor f where
   map :: forall a b. (a -> b) -> f a -> f b
-}

data Identity a = Identity a
derive instance functorIdentity :: Functor Identity

data Maybe a = Nothing | Just a

derive instance functorMaybe :: Functor Maybe

test :: Maybe String
test = map show (Just 1)

--------------------------------

n1 :: Maybe Int
n1 = Just 1

n2 :: Maybe Int
n2 = Just 2

sum :: Maybe Int
sum = case n1, n2 of
  Just n1', Just n2' -> Just (n1' + n2')
  _, _ -> Nothing

s1 :: Maybe String
s1 = Just "hello "

s2 :: Maybe String
s2 = Just "world"

concatenate :: Maybe String
concatenate = case s1, s2 of
    Just s1', Just s2' -> Just (s1' <> s2')
    _, _ -> Nothing

{-

n1 :: Maybe Int
n2 :: Maybe Int

(+) :: Int -> Int -> Int

map :: (a -> b) -> Maybe Int -> Maybe Int

(Int -> (Int -> Int)) -> Maybe a -> Maybe b
a ^        b ^

map (+) n1 :: Maybe (Int -> Int)
n2 :: Maybe Int

apply (map (+) n1) n2 :: Maybe Int

-}

-- class Functor f <= Apply' f where
--     apply' :: forall a b. f (a -> b) -> f a -> f b

instance applyMaybe :: Apply Maybe where
    apply :: forall a b. Maybe (a -> b) -> Maybe a -> Maybe b
    apply ma2b ma = case ma2b, ma of
        Just a2b, Just a -> Just (a2b a)
        _, _ -> Nothing

-- infixl ? map as <$>
-- infixl 4 apply' as <*>

sum' :: Maybe Int
sum' = (+) <$> n1 <*> n2

concat' :: Maybe String
concat' = (<>) <$> s1 <*> s2

add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z

sum3 :: Maybe Int
sum3 = add3 <$> n1 <*> n2 <*> pure 3

-- class Apply' f <= Applicative' f where
--     pure' :: forall a. a -> f a

instance applicativeMaybe :: Applicative Maybe where
    pure a = Just a

------------------------------

m1 :: Maybe Int
m1 = Just 1

op1 :: Int -> Maybe Int
op1 n = if n == 0 then Nothing else Just (n + 1)

op2 :: Int -> Maybe String
op2 10 = Nothing
op2 n  = Just (show n)

op3 :: String -> Maybe String
op3 s = if s == "hello" then Nothing else Just (s <> "!")

composition :: Maybe String
composition =
     case m1 of
        Nothing -> Nothing
        Just v1 -> case op1 v1 of
            Nothing -> Nothing
            Just v2 -> case op2 v2 of
                Nothing -> Nothing
                Just v3 -> op3 v3


fn :: forall a b. Maybe a -> (a -> Maybe b) -> Maybe b
fn ma a2mb = case ma of
    Nothing -> Nothing
    Just a  -> a2mb a

composition' :: Maybe String
composition' = m1 >>= op1 >>= op2 >>= op3

-- class Apply' m <= Bind' m where
--     bind' :: forall a b. m a -> (a -> m b) -> m b

instance maybeBind :: Bind Maybe where
    bind = fn

-- infixl 4 fn as >>=

-- class (Bind' m, Applicative' m) <= Monad m

composition'' :: Maybe String
composition'' = do
    a <- m1
    let a' = a + 10
    b <- op1 a'
    c <- op2 b
    d <- op3 c
    pure d

-----------------------------------------------------------------------------------------

data Either x y = Left x | Right y

derive instance functorEither :: Functor (Either x)

instance applyEither :: Apply (Either x) where
    apply :: forall x a b. Either x (a -> b) -> Either x a -> Either x b
    apply e_x_a2b e_x_a =
        case e_x_a2b, e_x_a of
            Right a2b, Right a -> Right (a2b a)
            Left x, _ -> Left x
            _, Left x -> Left x

instance applicativeEither :: Applicative (Either x) where
    pure :: forall a x. a -> Either x a
    pure a = Right a

instance bindEither :: Bind (Either x) where
    bind :: forall x a b. Either x a -> (a -> Either x b) -> Either x b
    bind e_x_a a2e_x_b = 
            case e_x_a of
                Left x -> Left x
                Right a -> a2e_x_b a

instance monadEither :: Monad (Either x)

e1 :: Either String Int
e1 = Right 1

f1 :: Int -> Either String Int
f1 x
   | x < 0 = Left "Cannot deal with negative numbers."
   | otherwise = Right (x * 2)

f2 :: Int -> Int -> Either String Int
f2 x y
   | x + y == 10 = Left "Cannot be 10"
   | otherwise   = Right (x + y)

f3 :: Int -> Either String String
f3 x
   | x > 100 = Left "Number too big."
   | otherwise = Right (show x)

test' :: Either String String
test' = do
    a <- e1
    b <- f1 a
    c <- f1 (a + 5)
    d <- f2 b c
    e <- f3 d
    pure e
