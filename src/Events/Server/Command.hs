module Events.Server.Command
  ( run
  ) where

import Options.Applicative

import Events.Config
import Events.Server (runServer)


newtype Command = Server ConfigOpts deriving (Show)


commandParser :: Parser Command
commandParser = hsubparser
  ( command "server" (info commandOptions (progDesc "Run the server")) )


commandOptions :: Parser Command
commandOptions = Server <$> configOptionsParser


configOptionsParser :: Parser ConfigOpts
configOptionsParser = ConfigOpts
  <$> option auto
      ( long "port"
      <> short 'p'
      <> metavar "PORT"
      <> help "Port to run the server on"
      <> value 8080
      <> showDefault )
  <*> strOption
      ( long "migrations"
      <> short 'm'
      <> metavar "DIR"
      <> help "Directory containing database migrations"
      <> value "/usr/local/share/events-mtaylor-io/migrations"
      <> showDefault )


delegate :: Command -> IO ()
delegate (Server conf) = getConfig conf >>= runServer


run :: IO ()
run = do
  cmd <- execParser opts
  delegate cmd
  where
    opts = info (commandParser <**> helper)
      ( fullDesc
      <> progDesc "Run the events-mtaylor-io server"
      <> header "events-mtaylor-io - a simple event management system" )
