module Ansible.Util where

import Ansible.Action
import Ansible.Shell
import Ansible.Types
import Data.Text (Text)

-- | Run ping module.
ping :: AnsibleCmd "ping"
ping =
  AnsibleCmd
    { ansibleModule = Module "ping",
      ansibleArgs = Nothing
    }

-- | Construct Ansible "shell" command.
shellCmd :: Text -> AnsibleCmd "shell"
shellCmd args = AnsibleCmd
  { ansibleModule = Module "shell",
    ansibleArgs = Just args
  }

-- | Run shell command.
runShell :: Text -> Pattern -> Ansible (Maybe ShellStdout)
runShell args target = do
  out <- runAdhoc (shellCmd args) target
  case out of
    Nothing -> return Nothing
    Just results -> return $ Just (shellOutput results)
