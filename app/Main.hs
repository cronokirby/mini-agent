module Main where

import Ourlude

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT (..), ask, liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import Network.HTTP.Simple as HTTP
import System.Environment (getEnv)

newtype APIKey = APIKey T.Text

instance Show APIKey where
  show _ = "APIKey \"<redacted>\""

apiKeyFromString :: String -> APIKey
apiKeyFromString = T.pack >>> APIKey

apiKeyToBS :: APIKey -> B.ByteString
apiKeyToBS (APIKey k) = T.encodeUtf8 k

readAPIKeyFromEnv :: IO APIKey
readAPIKeyFromEnv = getEnv "ANTHROPIC_API_KEY" |> fmap apiKeyFromString

newtype Anthropic a = Anthropic (ReaderT APIKey IO a)
  deriving newtype (Functor, Applicative, Monad, MonadReader APIKey, MonadIO)

runAnthropic :: Anthropic a -> APIKey -> IO a
runAnthropic (Anthropic eff) = runReaderT eff

runAnthropicWithEnv :: Anthropic a -> IO a
runAnthropicWithEnv eff = do
  key <- readAPIKeyFromEnv
  runAnthropic eff key

apiCall :: (ToJSON i, FromJSON o) => T.Text -> i -> Anthropic o
apiCall path apiCallBody = do
  key <- ask
  req <- mkRequest key apiCallBody
  resp <- HTTP.httpJSON req
  pure (HTTP.getResponseBody resp)
 where
  requestString = "POST https://api.anthropic.com" <> T.unpack path
  mkRequest :: (ToJSON i) => APIKey -> i -> Anthropic (HTTP.Request)
  mkRequest key body =
    liftIO (HTTP.parseRequest requestString) |> fmap (augment key body)
  augment key body =
    HTTP.setRequestHeader "anthropic-version" ["2023-06-01"]
      >>> HTTP.setRequestHeader "x-api-key" [apiKeyToBS key]
      >>> HTTP.setRequestBodyJSON body

data Role = User | Assistant

instance ToJSON Role where
  toJSON =
    Aeson.String <<< \case
      User -> "user"
      Assistant -> "assistant"

data Message = Message {role :: Role, content :: T.Text} deriving (Generic)

instance ToJSON Message

data MessagesRequest = MessagesRequest
  { model :: T.Text
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

apiMessages :: MessagesRequest -> Anthropic MessagesResponse
apiMessages req = apiCall "/v1/messages" req

defaultModel :: T.Text
defaultModel = "claude-3-5-haiku-20241022"

realMain :: Anthropic ()
realMain = do
  MessagesResponse content <- apiMessages (MessagesRequest defaultModel 8192 [Message User "Tell me a joke?"])
  for_ content <| \case
    TextContent t -> liftIO <| T.putStrLn t

main :: IO ()
main = runAnthropicWithEnv realMain
