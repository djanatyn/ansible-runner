module Ansible.Log
  ( -- * Types
    LogMessage,

    -- * Actions
    logMsg,
  )
where

import Ansible.Types (Ansible)
import Colog.Core.Action ((&>), (>$<), LogAction (..))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (hPutStrLn)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Data.Time.LocalTime (LocalTime (..), getTimeZone, utcToLocalTime)
import System.IO (stderr)

-- | Log inside an Ansible action:
--
-- @
-- >>> runAnsible (Config $ Inventory "localhost,") (logAnsibleStderr <& "hello world")
-- hello world
-- @
logAnsibleStderr :: LogAction Ansible T.Text
logAnsibleStderr = LogAction $ liftIO . TIO.hPutStrLn stderr

-- | Every `LogMessage` has a time it occurred.
data LogMessage
  = Message
      { time :: UTCTime,
        localTime :: LocalTime,
        message :: T.Text
      }

-- | Format LogMessage to `T.Text`. Displays local time.
instance Show LogMessage where
  show Message {localTime, message} = "[" ++ show localTime ++ "] " ++ T.unpack message

-- | Convert `UTCTime` to `LocalTime`.
getLocalTime :: UTCTime -> IO LocalTime
getLocalTime utcTime = do
  localTimeZone <- getTimeZone utcTime
  return $ utcToLocalTime localTimeZone utcTime

-- | Log inside an Ansible action:
--
-- @
-- >>> runAnsible (Config (Inventory "localhost,")) (logMsg "hello world")
-- [2019-11-14 20:22:50.224771] hello world
-- @
logMsg ::
  -- | Log message.
  T.Text ->
  Ansible ()
logMsg message = do
  time <- liftIO getCurrentTime
  localTime <- liftIO $ getLocalTime time
  Message {time, localTime, message} &> (T.pack . show >$< logAnsibleStderr)
