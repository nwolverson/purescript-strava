module Strava.Activity where

import Prelude
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Argonaut (class DecodeJson, JObject, Json, decodeJson, (.?))
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.JSDate (parse, toDateTime)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype)

data ActivityType =
  Run
  | Ride
  | Swim
  | Other String

instance decodeJsonActivityType :: DecodeJson ActivityType where
  decodeJson j = do
    s :: String <- decodeJson j
    pure $ case s of
      "Run" -> Run
      "Ride" -> Ride
      "Swim" -> Swim
      _ -> Other s

derive instance eqActivityType :: Eq ActivityType

newtype Metres = Metres Number
derive instance newtypeMetres :: Newtype Metres _
derive instance eqMetres :: Eq Metres

derive newtype instance semiringMetres :: Semiring Metres

instance decodeJsonMetres :: DecodeJson Metres where
  decodeJson = decodeJson >>> (map Metres)

newtype StravaDate = StravaDate DateTime
derive instance eqStravaDate :: Eq StravaDate
derive instance ordStravaDate :: Ord StravaDate
derive instance newtypeStravaDate :: Newtype StravaDate _

instance decodeJsonStravaDate :: DecodeJson StravaDate where
  decodeJson json = do
    s :: String <- decodeJson json
    -- Date is always ISO format
    let res = maybe (Left "Couldn't parse date") Right $ toDateTime $ unsafePerformEff $ parse s
    StravaDate <$> res

newtype Activity = Activity
  { id :: Int
  , name :: String
  , distance :: Metres
  , type' :: ActivityType
  , startDate :: StravaDate
}

derive instance newtypeActivity :: Newtype Activity _
derive instance eqActivity :: Eq Activity

instance decodeJsonActivity :: DecodeJson Activity where
  decodeJson (o :: Json) = do
    obj :: JObject <- decodeJson o
    id <- obj .? "id"
    name <- obj .? "name"
    distance <- obj .? "distance"
    type' <- obj .? "type"
    startDate <- obj .? "start_date"
    pure $ Activity { id, name, distance, type', startDate }
