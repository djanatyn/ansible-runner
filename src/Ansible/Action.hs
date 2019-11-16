module Ansible.Action
  ( -- * Interpreter
    runAnsible,

    -- * Executing Adhoc Commands
    runAdhoc,
  )
where

import Ansible.Log (logMsg)
import Ansible.Types (Ansible, AnsibleCmd (..), Config (..), Inventory (..), Module (..), Pattern)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks, runReaderT)
import qualified Data.Text as T
import System.Process.Typed (proc, readProcess)

-- | Run an adhoc `AnsibleCmd` given a host pattern.
runAdhoc :: AnsibleCmd -> Pattern -> Ansible ()
runAdhoc cmd target = do
  cmdline <- ansibleCmdline target cmd
  let action = proc "ansible" $ T.unpack <$> cmdline
   in do
        logMsg $ T.intercalate " " $ "$ ansible" : cmdline
        liftIO $ readProcess action >>= print

-- | Generate cmdline for an adhoc `AnsibleCmd`.
-- Does not include inventory.
ansibleCmdline :: Pattern -> AnsibleCmd -> Ansible [T.Text]
ansibleCmdline ansiblePattern cmd@AdhocCmd {ansibleModule = (Module m)} = do
  (Inventory path) <- asks ansibleInventory
  return $ ["-i", path, "-m", m] ++ ansibleArgsCmdline cmd ++ [ansiblePattern]

ansibleArgsCmdline :: AnsibleCmd -> [T.Text]
ansibleArgsCmdline (ansibleArgs -> Just args) = ["-a", args]
ansibleArgsCmdline _ = []

-- | Run `Ansible` action. An `Ansible` action needs an inventory to run against.
runAnsible :: Config -> Ansible a -> IO a
runAnsible config action = runReaderT action config
