module Control.LLM (LLM, query, runLLM) where

import Ourlude

import Bluefin.Compound (Handle (..), useImplIn)
import Bluefin.Eff (Eff, (:&), (:>))
import Bluefin.Free (Free, free, runFree)
import Bluefin.State (State, evalState, get, modify)
import qualified Data.Text as T

import Control.LLM.Interface (LLMInterface (..), Message (..), Role (..))

data LLM e = LLM
  { llmE :: Free LLMInterface e
  , stateE :: State [Message] e
  }

instance Handle LLM where
  mapHandle (LLM llmE stateE) =
    LLM
      { llmE = mapHandle llmE
      , stateE = mapHandle stateE
      }

query :: (e :> es) => LLM e -> T.Text -> Eff es [T.Text]
query (LLM llmE stateE) msg = do
  modify stateE ((Message User msg) :)
  messages <- fmap reverse (get stateE)
  out <- free llmE (Query messages)
  modify stateE ((out |> map (Message AI) |> reverse) ++)
  pure out

runLLM ::
  (forall r. LLMInterface r -> Eff es r) ->
  (forall e'. LLM e' -> Eff (e' :& es) a) ->
  Eff es a
runLLM handler k =
  evalState [] \stateE ->
    runFree handler \llmE ->
      useImplIn k (LLM (mapHandle llmE) (mapHandle stateE))
