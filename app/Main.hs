module Main where

import Ourlude

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)

-- | The possible responses returned by the Anthropic API.
data ResponseContent
  = TextResponse {text :: T.Text}
  deriving (Show, Generic)

responseContentJSONOptions :: Aeson.Options
responseContentJSONOptions =
  Aeson.defaultOptions
    { Aeson.tagSingleConstructors = True
    , Aeson.constructorTagModifier = \case
        "TextResponse" -> "text"
        x -> x
    , Aeson.sumEncoding =
        Aeson.TaggedObject
          { Aeson.tagFieldName = "type"
          , Aeson.contentsFieldName = "rest"
          }
    }

instance ToJSON ResponseContent where
  toJSON = Aeson.genericToJSON responseContentJSONOptions

instance FromJSON ResponseContent where
  parseJSON = Aeson.genericParseJSON responseContentJSONOptions

main :: IO ()
main = do
  x <- T.getLine
  let y = Aeson.decodeStrictText @ResponseContent x
  print y
  main
