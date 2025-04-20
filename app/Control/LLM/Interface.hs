module Control.LLM.Interface (
  Role (..),
  Message (..),
  LLMRequest (..),
  LLMResponse (..),
  LLMInterface,
  anthropicInterface,
)
where

import Network.Anthropic qualified as Anthropic
import Ourlude

import Data.Text qualified as T

data Role = User | AI

data Message = Message Role T.Text

newtype LLMRequest = LLMRequest [Message]

newtype LLMResponse = LLMResponse [T.Text]

type LLMInterface = LLMRequest -> IO LLMResponse

anthropicInterface :: Anthropic.APIKey -> LLMInterface
anthropicInterface key req =
  req |> convertRequest |> Anthropic.apiMessages key |> fmap convertResponse
 where
  convertRole :: Role -> Anthropic.Role
  convertRole User = Anthropic.User
  convertRole AI = Anthropic.Assistant
  convertMessage :: Message -> Anthropic.Message
  convertMessage (Message r t) = Anthropic.Message{role = convertRole r, content = t}
  convertRequest :: LLMRequest -> Anthropic.MessagesRequest
  convertRequest (LLMRequest msgs) = Anthropic.MessagesRequest Anthropic.Claude_3_5_Haiku 8192 (map convertMessage msgs)
  convertResponse :: Anthropic.MessagesResponse -> LLMResponse
  convertResponse resp =
    resp.content |> map (.text) |> LLMResponse
