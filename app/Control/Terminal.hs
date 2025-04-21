{-# LANGUAGE FieldSelectors #-}

module Control.Terminal (
  Terminal,
  userInput,
  aiOutput,
  runTerminalIO,
)
where

import Ourlude

import Bluefin.Compound (Handle)
import Bluefin.Eff (Eff, (:&), (:>))
import Bluefin.Free (Free, free, runFree)
import Bluefin.IO (IOE, effIO)
import Data.Coerce (coerce)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (hFlush, stdout)

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

runTerminalIO :: (e :> es) => IOE e -> (forall e'. Terminal e' -> Eff (e' :& es) a) -> Eff es a
runTerminalIO io = runTerminal (handler >>> effIO io)
 where
  handler :: forall r. TerminalF r -> IO r
  handler UserInput = do
    T.putStr "\x1b[94mMe: \x1b[0m"
    hFlush stdout
    T.getLine
  handler (AIOutput t) = do
    T.putStr "\x1b[92mAI: \x1b[0m"
    T.putStrLn t
