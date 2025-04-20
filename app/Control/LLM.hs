{-# LANGUAGE FieldSelectors #-}

module Control.LLM (LLM, query, runLLMIO) where

import Ourlude

import Bluefin.Compound (Handle (..), makeOp, useImplIn, useImplUnder)
import Bluefin.Eff (Eff, (:&), (:>))
import Bluefin.IO (IOE, effIO)
import Bluefin.State (State, evalState, get, modify)
import qualified Data.Text as T

import Control.LLM.Interface

data LLM e = LLM
  { requestImpl :: forall e'. LLMRequest -> Eff (e' :& e) LLMResponse
  , stateE :: State [Message] e
  }

instance Handle LLM where
  mapHandle l =
    LLM
      { requestImpl = \r -> useImplUnder (requestImpl l r)
      , stateE = mapHandle (stateE l)
      }

request :: (e :> es) => LLM e -> LLMRequest -> Eff es LLMResponse
request e = requestImpl (mapHandle e) >>> makeOp

getState :: (e :> es) => LLM e -> Eff es [Message]
getState e = get (stateE e)

modifyState :: (e :> es) => LLM e -> ([Message] -> [Message]) -> Eff es ()
modifyState e = modify (stateE e)

query :: (e :> es) => LLM e -> T.Text -> Eff es [T.Text]
query llm msg = do
  modifyState llm ((Message User msg) :)
  messages <- fmap reverse (getState llm)
  LLMResponse out <- request llm (LLMRequest messages)
  modifyState llm ((out |> map (Message AI) |> reverse) ++)
  pure out

runLLMIO :: (e :> es) => LLMInterface -> IOE e -> (forall e'. LLM e' -> Eff (e' :& es) a) -> Eff es a
runLLMIO interface io k =
  evalState [] $ \st ->
    useImplIn
      k
      ( LLM
          { requestImpl = effIO io <<< interface
          , stateE = mapHandle st
          }
      )
