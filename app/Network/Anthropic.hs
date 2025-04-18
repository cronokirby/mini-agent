module Network.Anthropic (
  APIKey (..),
  apiKeyFromString,
  Model (..),
  apiMessages,
  MessagesRequest (..),
  Role (..),
  Message (..),
  MessagesResponse (..),
  Content (..),
)
where

import Ourlude

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import Network.HTTP.Simple as HTTP

newtype APIKey = APIKey T.Text

instance Show APIKey where
  show _ = "APIKey \"<redacted>\""

apiKeyFromString :: String -> APIKey
apiKeyFromString = T.pack >>> APIKey

apiKeyToBS :: APIKey -> B.ByteString
apiKeyToBS (APIKey k) = T.encodeUtf8 k

apiCall :: (ToJSON i, FromJSON o, MonadIO m) => APIKey -> T.Text -> i -> m o
apiCall key path apiCallBody = do
  req <- mkRequest key apiCallBody
  resp <- HTTP.httpJSON req
  pure (HTTP.getResponseBody resp)
 where
  requestString = "POST https://api.anthropic.com" <> T.unpack path
  mkRequest :: (ToJSON i, MonadIO m) => APIKey -> i -> m HTTP.Request
  mkRequest key' body =
    liftIO (HTTP.parseRequest requestString) |> fmap (augment key' body)
  augment key' body =
    HTTP.setRequestHeader "anthropic-version" ["2023-06-01"]
      >>> HTTP.setRequestHeader "x-api-key" [apiKeyToBS key']
      >>> HTTP.setRequestBodyJSON body

data Role = User | Assistant

instance ToJSON Role where
  toJSON =
    Aeson.String <<< \case
      User -> "user"
      Assistant -> "assistant"

data Message = Message {role :: Role, content :: T.Text} deriving (Generic)

instance ToJSON Message

data Model = Claude_3_5_Haiku

instance ToJSON Model where
  toJSON =
    Aeson.String <<< \case
      Claude_3_5_Haiku -> "claude-3-5-haiku-20241022"

data MessagesRequest = MessagesRequest
  { model :: Model
  , max_tokens :: Int
  , messages :: [Message]
  }
  deriving (Generic)

instance ToJSON MessagesRequest

data Content
  = TextContent {text :: T.Text}
  deriving (Generic)

instance FromJSON Content where
  parseJSON = Aeson.genericParseJSON options
   where
    options :: Aeson.Options
    options =
      Aeson.defaultOptions
        { Aeson.tagSingleConstructors = True
        , Aeson.constructorTagModifier = \case
            "TextContent" -> "text"
            x -> x
        , Aeson.sumEncoding =
            Aeson.TaggedObject
              { Aeson.tagFieldName = "type"
              , Aeson.contentsFieldName = "rest"
              }
        }

-- | The possible responses returned by the Anthropic API.
data MessagesResponse = MessagesResponse {content :: [Content]}
  deriving (Generic)

instance FromJSON MessagesResponse

apiMessages :: (MonadIO m) => APIKey -> MessagesRequest -> m MessagesResponse
apiMessages key req = apiCall key "/v1/messages" req
