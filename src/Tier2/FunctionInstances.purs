module Tier2.FunctionInstances where

import Prelude

import Effect (Effect)
import Effect.Console (log)

data Fun a b = Fun (a -> b)

instance semigroupFunction :: Semigroup b => Semigroup (Fun a b) where
  append :: Fun a b -> Fun a b -> Fun a b
  append (Fun f) (Fun g) =
    Fun (\a -> f a <> g a)

instance monoidFunction :: Monoid b => Monoid (Fun a b) where
   mempty :: Fun a b
   mempty = Fun $ \_ -> mempty


data Fun1 a b = Fun1 (a -> b)

instance semigroupFun1 :: Semigroup (Fun1 a a) where
  append :: Fun1 a a -> Fun1 a a -> Fun1 a a
  append (Fun1 f) (Fun1 g) = Fun1 (f <<< g)

instance monoidFun1 :: Monoid (Fun1 a a) where
  mempty :: Fun1 a a
  mempty = Fun1 identity

example :: String
example = case Fun1 f <> Fun1 g of
    Fun1 result -> result "Andrew"
    where
      f :: String -> String
      f s = s <> "!"

      g :: String -> String
      g s = "Hello " <> s


instance functorFun :: Functor (Fun r) where
    map :: forall a b. (a -> b) -> Fun r a -> Fun r b
    map a2b (Fun r2a) =
        Fun $ \r -> a2b (r2a r) 

instance applyFun :: Apply (Fun r) where
   --                      r2a2b            r2a      r
   --                 . (r -> a -> b)  -> (r -> a)-> r -> b
   apply :: forall a b. Fun r (a -> b) -> Fun r a -> Fun r b
   apply (Fun r2a2b) (Fun r2a) =
       Fun $ \r -> r2a2b r (r2a r)
        --    let a = r2a r
        --     in r2a2b r a

instance applicativeFun :: Applicative (Fun r) where
    pure :: forall a. a -> Fun r a
    pure a = Fun $ \_ -> a

instance bindFun :: Bind (Fun r) where
    --                  (r -> a)-> (a -> r -> b)  -> r -> b
    bind :: forall a b. Fun r a -> (a -> Fun r b) -> Fun r b
    bind (Fun r2a) a2Funrb =
        Fun $ \r -> case a2Funrb (r2a r) of
             (Fun r2b) -> r2b r

instance monadFun :: Monad (Fun r)

-----------------------------------------------

-- url :: String

pingServer' :: String -> Effect Unit
pingServer' url = log $ "pinging server" <> url

restartServer' :: String -> Effect Unit
restartServer' url = log $ "restarting " <> url

killServer' :: String -> Effect Unit
killServer' url = log $ "killing " <> url

doSomethingElse' :: String -> Effect Unit
doSomethingElse' url = do
    pingServer' url
    killServer' url
    pingServer' url
    restartServer' url

main' :: Effect Unit
main' = do
    let url = "whatever"
    pingServer' url
    restartServer' url
    pingServer' url

-- Removing duplication
data ReaderT r m a = ReaderT (r -> m a)

instance functorReader :: Functor m => Functor (ReaderT r m) where
    map :: forall a b. (a -> b) -> ReaderT r m a -> ReaderT r m b
    map f (ReaderT r2ma) = 
       ReaderT $ \r ->
           let ma = r2ma r
           in map f ma

instance applyReader :: Apply m => Apply (ReaderT r m) where
    apply :: forall a b. ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
    apply (ReaderT r2ma2b) (ReaderT r2ma) =
        ReaderT $ \r ->
            let ma = r2ma r -- m a
                ma2b = r2ma2b r -- m (a -> b)
            in apply ma2b ma

instance applicativeReader :: Applicative m => Applicative (ReaderT r m) where
    pure :: forall a. a -> ReaderT r m a
    pure a = ReaderT $ \_ -> pure a

instance bindReader :: Bind m => Bind (ReaderT r m) where
    bind :: forall a b. ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    bind (ReaderT r2ma) a2RTrmb =
        ReaderT $ \r ->
            let ma = r2ma r
                go (ReaderT r2mb) = r2mb r
            in bind ma (go <<< a2RTrmb)

instance monadReader :: (Bind m, Applicative m) => Monad (ReaderT r m)

pingServer :: ReaderT String Effect Unit
pingServer = ReaderT $ \url -> log $ "pinging server" <> url

restartServer :: ReaderT String Effect Unit
restartServer = ReaderT $ \url -> log $ "restarting " <> url

killServer :: ReaderT String Effect Unit
killServer = ReaderT $ \url -> log $ "killing " <> url

doSomethingElse :: ReaderT String Effect Unit
doSomethingElse = do
    pingServer
    killServer
    pingServer
    restartServer
    lift $ log "done"
    url <- ask
    lift $ log $ "done for url: " <> url

lift :: forall r m a. m a -> ReaderT r m a
lift ma = ReaderT $ \_ -> ma

ask :: forall r m. Applicative m => ReaderT r m r
ask = ReaderT $ \r -> pure r

runReaderT :: forall r m a. r -> ReaderT r m a -> m a
runReaderT r (ReaderT r2ma) = r2ma r

main :: Effect Unit
main = do
    let url = "whatever"
    runReaderT url $ do
        pingServer
        restartServer
        pingServer 
        doSomethingElse