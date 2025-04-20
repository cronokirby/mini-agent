{-# LANGUAGE FieldSelectors #-}

module Control.Terminal (
  Terminal,
  userInput,
  aiOutput,
  runTerminalIO,
)
where

import Ourlude

import Bluefin.Compound (Handle (..), makeOp, useImplIn, useImplUnder)
import Bluefin.Eff (Eff, (:&), (:>))
import Bluefin.IO (IOE, effIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (hFlush, stdout)

data Terminal e = Terminal
  { userInputImpl :: forall e'. Eff (e' :& e) T.Text
  , aiOutputImpl :: forall e'. T.Text -> Eff (e' :& e) ()
  }

instance Handle Terminal where
  mapHandle t =
    Terminal
      { userInputImpl = useImplUnder (userInputImpl t)
      , aiOutputImpl = \out -> useImplUnder (aiOutputImpl t out)
      }

userInput :: (e :> es) => Terminal e -> Eff es T.Text
userInput e = userInputImpl (mapHandle e) |> makeOp

aiOutput :: (e :> es) => Terminal e -> T.Text -> Eff es ()
aiOutput e = aiOutputImpl (mapHandle e) >>> makeOp

runTerminalIO :: (e :> es) => IOE e -> (forall e'. Terminal e' -> Eff (e' :& es) a) -> Eff es a
runTerminalIO io k =
  useImplIn
    k
    ( Terminal
        { userInputImpl =
            effIO io <| do
              T.putStr "\x1b[94mMe: \x1b[0m"
              hFlush stdout
              T.getLine
        , aiOutputImpl = \t ->
            effIO io <| do
              T.putStr "\x1b[92mAI: \x1b[0m"
              T.putStrLn t
        }
    )
