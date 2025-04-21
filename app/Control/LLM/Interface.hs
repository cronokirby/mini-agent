module Control.LLM.Interface (
  Role (..),
  Message (..),
  LLMInterface (..),
)
where

import Data.Text qualified as T

data Role = User | AI

data Message = Message Role T.Text

data LLMInterface a where
  Query :: [Message] -> LLMInterface [T.Text]
