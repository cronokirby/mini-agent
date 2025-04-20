module Main where

import Ourlude

import Bluefin.Eff (Eff, runEff_, (:>))
import Data.Foldable (for_)
import System.Environment (getEnv)

import Control.LLM
import Control.LLM.Interface (anthropicInterface)
import Control.Terminal
import Network.Anthropic (APIKey, apiKeyFromString)

readAPIKeyFromEnv :: IO APIKey
readAPIKeyFromEnv = getEnv "ANTHROPIC_API_KEY" |> fmap apiKeyFromString

realMain :: (e0 :> es, e1 :> es) => LLM e0 -> Terminal e1 -> Eff es ()
realMain llm term = do
  input <- userInput term
  responses <- query llm input
  for_ responses (aiOutput term)
  realMain llm term

main :: IO ()
main = do
  key <- readAPIKeyFromEnv
  runEff_ $ \io ->
    runLLMIO (anthropicInterface key) io $ \llm ->
      runTerminalIO io $ \term -> realMain llm term
