module Control.LLM.Interface (
  Role (..),
  Message (..),
  Request (..),
  Response (..),
  LLMInterface (..),
)
where

import qualified Data.Text as T

data Role = User | AI

data Message = Message Role T.Text

newtype Request = Request [Message]

newtype Response = Response [T.Text]

data LLMInterface m = LLMInterface
  { request :: Request -> m Response
  }
