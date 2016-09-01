module Main
  ( main
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
import Data.String.Conversions (cs)
import Network.Nats (withNats, defaultSettings, publish)
import Options.Applicative
import System.IO

data Options = Options
  { natsUri :: !String
  , topic   :: !String
  , replyTo :: !(Maybe String)
  , payload :: !String
  } deriving Show

main :: IO ()
main = do
  opts <- getOptions
  withNats defaultSettings [natsUri opts] $ \nats -> do
    publish nats (cs $ topic opts) (cs <$> replyTo opts) (cs $ payload opts)
    waitAWhile

waitAWhile :: IO ()
waitAWhile = do
  replicateM_ 10 $ do
      putChar '.'
      hFlush stdout
      threadDelay 50000
  putChar '\n'

getOptions :: IO Options
getOptions = execParser options

options :: ParserInfo Options
options =
  info (helper <*> optParser)
    ( fullDesc
    <> progDesc "Publish a NATS message."
    <> header "NATS CLI client - publish."
    )

optParser :: Parser Options
optParser =
  Options <$> option auto
    ( long "nats"
      <> short 'n'
      <> metavar "<NATS URI>"
      <> help "NATS URI for connecting to NATS."
      <> value "nats://localhost:4222"
    )
          <*> strOption
    ( long "topic"
      <> short 't'
      <> metavar "<TOPIC>"
      <> help "TOPIC where to publish."
    )
          <*> (optional $ strOption
    ( long "reply"
      <> short 'r'
      <> metavar "<REPLY-TO>"
      <> help "Optional REPLY-TO topic where reply are sent."
    ))
          <*> strOption
    ( long "payload"
      <> short 'p'
      <> metavar "<PAYLOAD>"
      <> help "PAYLOAD to publish."
    )
