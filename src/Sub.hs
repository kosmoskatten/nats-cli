module Main
  ( main
  ) where

import Data.String.Conversions (cs)
import Options.Applicative
import Network.Nats ( Msg, withNats, defaultSettings, subscribe
                    , unsubscribe, nextMsg, topic, replyTo, payload
                    )

data Options = Options
  { natsUri    :: !String
  , topic'     :: !String
  , queueGroup :: !(Maybe String)
  } deriving Show

main :: IO ()
main = do
  opts <- getOptions
  withNats defaultSettings [natsUri opts] $ \nats -> do
    (sid, queue) <- subscribe nats (cs $ topic' opts) (cs <$> queueGroup opts)
    printReply =<< nextMsg queue
    unsubscribe nats sid Nothing

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
    <> progDesc "Subscribe to a NATS topic."
    <> header "NATS CLI client - subscribe."
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
      <> help "TOPIC to subscribe."
    )
          <*> (optional $ strOption
    ( long "qgroup"
      <> short 'q'
      <> metavar "<QUEUE-GROUP>"
      <> help "Optional QUEUE-GROUP to participate in."
    ))

