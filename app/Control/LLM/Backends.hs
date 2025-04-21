module Control.LLM.Backends (
  APIKey,
  anthropic,
) where

import Ourlude

import Network.Anthropic qualified as Anthropic

import Control.LLM.Interface (LLMInterface (..), Message (..), Role (..))
import Data.ByteString qualified as B

type APIKey = B.ByteString

anthropic :: APIKey -> (forall r. LLMInterface r -> IO r)
anthropic key = \case
  Query msgs -> do
    let req = Anthropic.MessagesRequest Anthropic.Claude_3_5_Haiku 8192 (map convertMessage msgs)
    resp <- Anthropic.apiMessages key req
    resp.content |> map (.text) |> pure
 where
  convertRole :: Role -> Anthropic.Role
  convertRole User = Anthropic.User
  convertRole AI = Anthropic.Assistant
  convertMessage :: Message -> Anthropic.Message
  convertMessage (Message r t) = Anthropic.Message{role = convertRole r, content = t}
