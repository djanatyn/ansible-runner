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

    -- * Output
    AdhocOutput (..),
  )
where

import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

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
newtype Module = Module T.Text deriving (Eq, Show)

-- | Adhoc commands need a `Module` and have optional arguments.
data AnsibleCmd
  = AnsibleCmd
      { -- | Module to invoke.
        ansibleModule :: Module,
        -- | Optional arguments to module.
        ansibleArgs :: Maybe T.Text
      }

newtype AdhocOutput = AdhocOutput {adhocOutput :: BL.ByteString} deriving (Show)
