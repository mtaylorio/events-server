module Command
  ( run
  ) where

import Options.Applicative

import Server (runServer)


data Command = Server deriving (Show)


commandParser :: Parser Command
commandParser = subparser
  ( command "server" (info (pure Server) (progDesc "Run the server")) )


delegate :: Command -> IO ()
delegate Server = runServer


run :: IO ()
run = do
  cmd <- execParser (info commandParser fullDesc)
  delegate cmd
