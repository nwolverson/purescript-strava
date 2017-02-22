module Strava.API.Activities where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (decodeJson)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (fromDateTime, unInstant)
import Data.Either (either)
import Data.Newtype (class Newtype, unwrap)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.StatusCode (StatusCode(..))
import Strava.Activity (Activity)

newtype AuthToken = AuthToken String
derive instance newtypeAuthToken :: Newtype AuthToken _

newtype ID = ID String
derive instance newtypeID :: Newtype ID _

getByID :: forall e. AuthToken -> ID -> Aff (ajax :: AJAX | e) Activity
getByID token id = do
  let prefix = "https://www.strava.com/api/v3/activities/"
  res <- affjax $ (authRequest token) { url = prefix <> unwrap id }
  either (throwError <<< error) pure (decodeJson res.response)

-- | Specify what set of activities to request. Sorted newest first, except
-- | for After which will be sorted oldest first.
data ActivityRangeStart = Default | Before DateTime | After DateTime | Page Int
type GetManyOpts =
  { pageCount :: Int
  , start :: ActivityRangeStart
  }

unixTime :: DateTime -> Number
unixTime = unwrap <<< unInstant <<< fromDateTime

authRequest :: AuthToken -> AffjaxRequest Unit
authRequest token = defaultRequest
  { headers = [ RequestHeader "Authorization" ("Bearer " <> unwrap token) ] }

getMany :: forall e. AuthToken -> GetManyOpts -> Aff (ajax :: AJAX | e) (Array Activity)
getMany token { pageCount, start } = do
  let prefix = "https://www.strava.com/api/v3/athlete/activities?per_page=" <> show pageCount
  let request = case start of
                  Default -> prefix
                  Page n -> prefix <> "&page=" <> show n
                  Before d -> prefix <> "&before=" <> show (unixTime d)
                  After d -> prefix <> "&after=" <> show (unixTime d)
  res <- affjax ( (authRequest token) { url = request })
  when (res.status /= StatusCode 200) $
    throwError $ error $ "Bad status code: " <> show res.status
  either (throwError <<< error) pure (decodeJson res.response)
