module Main where

import Ansible.Types (AnsibleCmd (..), Module (..))
import Data.Text (Text)

-- | Run ping module.
ping :: AnsibleCmd
ping =
  AnsibleCmd
    { ansibleModule = Module "ping",
      ansibleArgs = Nothing
    }

-- | Run shell command.
shell :: Text -> AnsibleCmd
shell args =
  AnsibleCmd
    { ansibleModule = Module "shell",
      ansibleArgs = Just args
    }

main :: IO ()
main = undefined
