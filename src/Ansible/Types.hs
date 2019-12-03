module Ansible.Types
  ( -- * Environment
    Ansible,
    LogLevel (..),
    Config (..),
    Inventory (..),

    -- * Actions
    AnsibleCmd (..),
    Module (..),
    Pattern,

    -- * Results
    Play (..),
    Task (..),
    Results (..),
    Host,
    HostResult,
    adhocResults,
  )
where

import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import GHC.TypeLits (Symbol)

-- A `Pattern` matches a host (or set of hosts) in an `Inventory`.
type Pattern = T.Text

-- | An `Inventory` is specified by a filepath.
data Inventory = Inventory T.Text | Localhost

-- | Levels for logs.
data LogLevel = FATAL | ERROR | WARN | INFO | DEBUG deriving (Show, Eq, Ord)

-- | Ansible runs against exactly one `Inventory` each invocation.
data Config
  = Config
      { ansibleInventory :: Inventory,
        ansibleLogLevel :: LogLevel
      }

-- | Ansible action type. Captures Ansible actions running against an
-- `Inventory`.
type Ansible = ReaderT Config IO

-- | Modules are represented as `T.Text`.
newtype Module (m :: Symbol) = Module T.Text deriving (Eq, Show)

-- | Adhoc commands need a `Module` and have optional arguments.
data AnsibleCmd (m :: Symbol)
  = AnsibleCmd
      { -- | Module to invoke.
        ansibleModule :: Module m,
        -- | Optional arguments to module.
        ansibleArgs :: Maybe T.Text
      }

newtype Results (m :: Symbol)
  = Results
      {resultPlays :: [Play]}
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

instance FromJSON (Results a) where
  parseJSON = withObject "results" $ \results -> do
    resultPlays <- results .: "plays"
    return Results {resultPlays}

adhocResults :: Results a -> [HostResult]
adhocResults Results {resultPlays} = do
  play <- resultPlays
  task <- playTasks play
  taskResults task
