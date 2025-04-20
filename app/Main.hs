module Main where

import Ourlude

import Bluefin.Eff (Eff, runEff_, (:>))
import Bluefin.IO (IOE, effIO)
import Data.Foldable (for_)
import System.Environment (getEnv)

import Control.Terminal
import Network.Anthropic

readAPIKeyFromEnv :: IO APIKey
readAPIKeyFromEnv = getEnv "ANTHROPIC_API_KEY" |> fmap apiKeyFromString

mkRequest :: [Message] -> MessagesRequest
mkRequest = MessagesRequest Claude_3_5_Haiku 8192

mainLoop :: (e0 :> es, e1 :> es) => APIKey -> [Message] -> IOE e0 -> Terminal e1 -> Eff es ()
mainLoop key messages io term = do
  input <- userInput term
  let messages' = Message User input : messages
      request = mkRequest (reverse messages')
  MessagesResponse{content} <- effIO io (apiMessages key request)
  let texts = map (\(TextContent t) -> t) content
  for_ texts (aiOutput term)
  let messages'' = map (Message Assistant) (reverse texts) ++ messages'
  mainLoop key messages'' io term

main :: IO ()
main = do
  key <- readAPIKeyFromEnv
  runEff_ (\io -> runTerminalIO io (mainLoop key [] io))
