module Main where

import Ourlude

import Bluefin.Eff (Eff, runEff_, (:>))
import Bluefin.IO (effIO)
import qualified Data.ByteString.Char8 as B
import Data.Foldable (for_)
import qualified Data.Text.IO as T
import System.Environment (getEnv)
import System.IO (hFlush, stdout)

import Control.LLM (LLM, query, runLLM)
import Control.LLM.Backends (APIKey, anthropic)
import Control.LLM.Interface (LLMInterface)
import Control.Terminal (
  Terminal,
  TerminalF (..),
  aiOutput,
  runTerminal,
  userInput,
 )

realMain :: (e0 :> es, e1 :> es) => LLM e0 -> Terminal e1 -> Eff es ()
realMain llm term = do
  input <- userInput term
  responses <- query llm input
  for_ responses (aiOutput term)
  realMain llm term

terminalHandler :: forall r. TerminalF r -> IO r
terminalHandler UserInput = do
  T.putStr "\x1b[94mMe: \x1b[0m"
  hFlush stdout
  T.getLine
terminalHandler (AIOutput t) = do
  T.putStr "\x1b[92mAI: \x1b[0m"
  T.putStrLn t

readAPIKeyFromEnv :: IO APIKey
readAPIKeyFromEnv = getEnv "ANTHROPIC_API_KEY" |> fmap B.pack

llmHandler :: APIKey -> (forall r. LLMInterface r -> IO r)
llmHandler = anthropic

main :: IO ()
main = do
  key <- readAPIKeyFromEnv
  runEff_ \io ->
    runTerminal (terminalHandler >>> effIO io) \term ->
      runLLM (llmHandler key >>> effIO io) \llm ->
        realMain llm term
