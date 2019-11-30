module Ansible.Parse (Play (..), AdhocOutput (..)) where

import Data.Aeson
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import GHC.TypeLits (Symbol)

newtype AdhocOutput (m :: Symbol)
  = AdhocOutput
      {adhocPlays :: [Play]}
  deriving (Show)

data Play
  = Play
      { playID :: T.Text,
        playName :: T.Text,
        playTasks :: [Task]
      }
  deriving (Show)

type Host = T.Text

type HostResult = (Host, Value)

data Task
  = Task
      { taskID :: T.Text,
        taskName :: T.Text,
        taskResults :: [HostResult]
      }
  deriving (Show)

instance FromJSON Task where
  parseJSON = withObject "task" $ \task -> do
    metadata <- task .: "task"
    taskID <- metadata .: "id"
    taskName <- metadata .: "name"
    taskResults <- HM.toList <$> (task .: "hosts" :: A.Parser A.Object)
    return Task {taskID, taskName, taskResults}

instance FromJSON Play where
  parseJSON = withObject "play" $ \play -> do
    metadata <- play .: "play"
    playID <- metadata .: "id"
    playName <- metadata .: "name"
    playTasks <- play .: "tasks"
    return Play {playID, playName, playTasks}

instance FromJSON (AdhocOutput a) where
  parseJSON = withObject "results" $ \results -> do
    adhocPlays <- results .: "plays"
    return AdhocOutput {adhocPlays}
