module Part10 where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Array (head, index)
import Data.Either (Either(..), hush)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log, logShow)

-- (Int -> String)         Array Int        Array String
-- show              <$>   [ 1, 2, 3 ] => ["1", "2", "3"]

-- String -> Aff (Maybe MarketChart)       Array String                 Array (Aff (Maybe MarketChart))
-- getMarketChart                      <$> [ "bitcoin", "ethereum" ]

-- traverse getMarketChart [ ... ] :: Aff (Array (Maybe MarketChart))

-- traverse f values = for values f

main :: Effect Unit
main = launchAff_ do
    let coins = [ "bitcoin", "ethereum", "cardano" ]
    -- This can also be written as: traverse getMarketChart coins
    results <- for coins getMarketChart
    logShow results
    pure unit

type CoinInfo =
    { coinId :: String
    , high :: Number
    , low :: Number
    }

data NotificationType
    = High
    | Low

type Notification =
    { mode :: NotificationType
    , coinId :: String
    , price :: Number
    }

getNotification :: String -> MarketChart -> List CoinInfo -> Maybe Notification
getNotification coinId (MarketChart inner) coinInfos = Nothing

getMarketChart :: String -> Aff (Maybe MarketChart)
getMarketChart coinId = do
  result <-
      AX.get
          ResponseFormat.json
          $ "https://api.coingecko.com/api/v3/coins/" <> coinId <> "/market_chart?vs_currency=eur&days=0"
  case result of
    Left err -> do
        log $ "GET /api response failed to decode: " <> AX.printError err
        pure Nothing
    Right response ->
        pure $ hush (decodeJson response.body)

printResult :: Either JsonDecodeError MarketChart -> String
printResult = case _ of
    Left _ -> "decoding error"
    Right (MarketChart inner) -> show inner.prices

newtype MarketChart = MarketChart
    { prices :: Number
    , market_caps :: Number
    , total_volumes :: Number
    }

instance showMarketChart :: Show MarketChart where
  show (MarketChart inner) = show inner

findNumber :: Array (Array Number) -> Either JsonDecodeError Number
findNumber arr = case head arr >>= (\child -> index child 1)  of
    Nothing -> Left $ TypeMismatch "Could not find number."
    Just n -> Right n

instance decodeJsonMarketChart :: DecodeJson MarketChart where
    decodeJson json = do
      object <- decodeJson json
      original_prices <- object .: "prices"
      prices <- findNumber original_prices
      original_market_caps <- object .: "market_caps"
      market_caps <- findNumber original_market_caps
      original_total_volumes <- object .: "total_volumes"
      total_volumes <- findNumber original_total_volumes
      pure $ MarketChart { prices, market_caps, total_volumes }
      


