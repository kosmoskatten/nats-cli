module Main
  ( main
  ) where

import Data.String.Conversions (cs)
import Network.Nats ( Msg, withNats, defaultSettings, request
                    , topic, replyTo, payload
                    )
import Options.Applicative

data Options = Options
  { natsUri  :: !String
  , topic'   :: !String
  , payload' :: !String
  } deriving Show

main :: IO ()
main = do
  opts <- getOptions
  withNats defaultSettings [natsUri opts] $ \nats ->
    printReply =<< request nats (cs $ topic' opts) (cs $ payload' opts)

printReply :: Msg -> IO ()
printReply msg = do
  putStrLn $ "Received topic: " ++ (cs $ topic msg)
  putStrLn $ "Reply-to: " ++ (maybe "" cs $ replyTo msg)
  putStrLn $ "Received payload: " ++ (cs $ payload msg)

getOptions :: IO Options
getOptions = execParser options

options :: ParserInfo Options
options =
  info (helper <*> optParser)
    ( fullDesc
    <> progDesc "Publish a NATS message, and wait for reply."
    <> header "NATS CLI client - request."
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
          <*> strOption
    ( long "payload"
      <> short 'p'
      <> metavar "<PAYLOAD>"
      <> help "PAYLOAD to publish."
    )
  
