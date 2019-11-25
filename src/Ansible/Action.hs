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

-- | Run an adhoc `AnsibleCmd` given a host pattern.
runAdhoc :: AnsibleCmd -> Pattern -> Ansible AdhocOutput
runAdhoc cmd target = do
  process <- ansibleProc target cmd
  env <- ansibleEnv
  logMsg INFO process
  let action = setEnv env process
   in liftIO $ AdhocOutput . snd <$> readProcessStdout action

-- | Environment for an `Ansible` adhoc process.
type Environment = [(String, String)]

-- | Construct `Ansible` environment. Sets STDOUT callback.
ansibleEnv :: Ansible Environment
ansibleEnv = do
  currentEnvironment <- liftIO getEnvironment
  let newArgs =
        [ ("ANSIBLE_STDOUT_CALLBACK", "json"),
          ("ANSIBLE_LOAD_CALLBACK_PLUGINS", "TRUE")
        ]
      currentArgs = filter ((`notElem` (fst <$> newArgs)) . fst) currentEnvironment
   in return $ newArgs ++ currentArgs

-- | Construct `ProcessConfig` for Ansible adhoc command.
ansibleProc :: Pattern -> AnsibleCmd -> Ansible (ProcessConfig () () ())
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
