module Main where

import Ansible.Action
import Ansible.Types
import Data.Text (Text)

-- | Run ping module.
ping :: AnsibleCmd "ping"
ping =
  AnsibleCmd
    { ansibleModule = Module "ping",
      ansibleArgs = Nothing
    }

-- | Run shell command.
shell :: Text -> AnsibleCmd "shell"
shell args =
  AnsibleCmd
    { ansibleModule = Module "shell",
      ansibleArgs = Just args
    }

main :: IO ()
main = undefined
