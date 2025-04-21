{-# LANGUAGE FieldSelectors #-}

module Control.Terminal (
  Terminal,
  TerminalF (..),
  userInput,
  aiOutput,
  runTerminal,
)
where

import Ourlude

import Bluefin.Compound (Handle)
import Bluefin.Eff (Eff, (:&), (:>))
import Bluefin.Free (Free, free, runFree)
import Data.Coerce (coerce)
import qualified Data.Text as T

data TerminalF a where
  UserInput :: TerminalF T.Text
  AIOutput :: T.Text -> TerminalF ()

newtype Terminal e = Terminal (Free TerminalF e)
  deriving newtype (Handle)

userInput :: (e :> es) => Terminal e -> Eff es T.Text
userInput e = free (coerce e) UserInput

aiOutput :: (e :> es) => Terminal e -> T.Text -> Eff es ()
aiOutput e = free (coerce e) <<< AIOutput

runTerminal :: (forall r. TerminalF r -> Eff es r) -> (forall e'. Terminal e' -> Eff (e' :& es) a) -> Eff es a
runTerminal h k = runFree h (coerce k)
