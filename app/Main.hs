module Main where

import Ourlude

import Data.Foldable (for_)
import qualified Data.Text.IO as T
import System.Environment (getEnv)
import System.IO (hFlush, stdout)

import Network.Anthropic

readAPIKeyFromEnv :: IO APIKey
readAPIKeyFromEnv = getEnv "ANTHROPIC_API_KEY" |> fmap apiKeyFromString

main :: IO ()
main = do
  key <- readAPIKeyFromEnv
  go key []
 where
  mkRequest = MessagesRequest Claude_3_5_Haiku 8192
  go :: APIKey -> [Message] -> IO ()
  go key messages = do
    T.putStr "\x1b[94mMe: \x1b[0m"
    hFlush stdout
    input <- T.getLine
    let messages' = Message User input : messages
    MessagesResponse{content} <- mkRequest (reverse messages') |> apiMessages key
    let texts = map (\(TextContent t) -> t) content
    for_ texts <| \t -> do
      T.putStr "\x1b[92mAI: \x1b[0m"
      T.putStrLn t
    go key <| map (Message Assistant) (reverse texts) ++ messages'
