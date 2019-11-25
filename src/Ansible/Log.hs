module Ansible.Log
  ( -- * Types
    LogMessage,

    -- * Log Levels
    LogLevel (..),

    -- * Actions
    logMsg,
  )
where

import Ansible.Types (Ansible)
import Colog.Core.Action ((&>), LogAction (..))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (hPutStrLn)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Data.Time.LocalTime (LocalTime (..), getTimeZone, utcToLocalTime)
import System.IO (stderr)

-- | Ansible logging action.
logAnsibleStderr :: Show a => LogAction Ansible a
logAnsibleStderr = LogAction $ liftIO . TIO.hPutStrLn stderr . T.pack . show

-- | Levels for logs.
data LogLevel = ERROR | WARN | INFO | TRACE deriving (Show)

-- | Timestamped `LogMessage` with severity.
data LogMessage a
  = Message
      { logTime :: UTCTime,
        logLocalTime :: LocalTime,
        logLevel :: LogLevel,
        logMessage :: a
      }

-- | Format LogMessage to `T.Text`. Displays local time.
instance Show a => Show (LogMessage a) where
  show Message {logLevel, logLocalTime, logMessage} =
    unwords
      [ "[" ++ show logLocalTime ++ "]",
        "(" ++ show logLevel ++ ")",
        show logMessage
      ]

-- | Convert `UTCTime` to `LocalTime`.
getLocalTime :: UTCTime -> IO LocalTime
getLocalTime utcTime = do
  localTimeZone <- getTimeZone utcTime
  return $ utcToLocalTime localTimeZone utcTime

-- | `LogMessage` smart constructor.
message :: Show a => LogLevel -> a -> Ansible (LogMessage a)
message logLevel logMessage = do
  logTime <- liftIO getCurrentTime
  logLocalTime <- liftIO $ getLocalTime logTime
  return Message {logTime, logLocalTime, logLevel, logMessage}

-- | Exported logging interface.
logMsg :: Show a => LogLevel -> a -> Ansible ()
logMsg level msg = message level msg >>= (&> logAnsibleStderr)
