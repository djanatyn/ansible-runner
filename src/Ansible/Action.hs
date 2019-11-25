module Ansible.Action
  ( -- * Interpreter
    runAnsible,

    -- * Executing Adhoc Commands
    runAdhoc,
  )
where

import Ansible.Log (LogLevel (..), logMsg)
import Ansible.Types
  ( AdhocOutput (..),
    Ansible,
    AnsibleCmd (..),
    Config (..),
    Inventory (..),
    Module (..),
    Pattern,
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks, runReaderT)
import Data.Coerce
import qualified Data.Text as T
import System.Environment (getEnvironment)
import System.Process.Typed (ProcessConfig, proc, readProcessStdout, setEnv)

-- | Ansible process.
type Process = ProcessConfig () () ()

-- | Environment variable.
type EnvVar = (String, String)

-- | Process environment.
type Environment = [EnvVar]

-- | Base environment for Ansible adhoc commands.
-- JSON stdout callback is used.
baseEnv :: Environment
baseEnv =
  [ ("ANSIBLE_STDOUT_CALLBACK", "json"),
    ("ANSIBLE_LOAD_CALLBACK_PLUGINS", "TRUE")
  ]

-- | Run an adhoc `AnsibleCmd` given a host pattern.
runAdhoc :: AnsibleCmd -> Pattern -> Ansible AdhocOutput
runAdhoc cmd target = do
  process <- ansibleProc target cmd
  logMsg WARN process
  command <- ansibleEnv process
  liftIO $ AdhocOutput . snd <$> readProcessStdout command

-- | Whether to override an existing environment variable.
overrideVar :: EnvVar -> Bool
overrideVar var = fst var `notElem` (fst <$> baseEnv)

-- | Set environment for `Ansible Process`.
ansibleEnv :: Process -> Ansible Process
ansibleEnv process = do
  currentEnvironment <- liftIO getEnvironment
  return $ setEnv (baseEnv ++ filter overrideVar currentEnvironment) process

-- | Construct `ProcessConfig` for Ansible adhoc command.
ansibleProc :: Pattern -> AnsibleCmd -> Ansible Process
ansibleProc ansiblePattern AnsibleCmd {ansibleArgs, ansibleModule} = do
  inv <- asks ansibleInventory
  return $ proc "ansible" . fmap T.unpack $
    let inventory = case inv of
          Inventory path -> ["-i", path]
          _ -> []
        modules = ["-m", coerce ansibleModule]
        args = case ansibleArgs of
          Just a -> ["-a", a]
          _ -> []
        target = [ansiblePattern]
     in inventory ++ modules ++ args ++ target

-- | Run `Ansible` action. An `Ansible` action needs an inventory to run against.
runAnsible :: Config -> Ansible a -> IO a
runAnsible config action = runReaderT action config
