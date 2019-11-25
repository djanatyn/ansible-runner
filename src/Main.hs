-- |
-- Module      : Main
-- Description : Run Ansible adhoc commands.
--
-- This module exports functions for constructing Ansible adhoc actions, and
-- running those actions.
--
-- >>> runAnsible (Config (Inventory "~/inv/ansible-runner.yml")) (runAdhoc (shell "whoami") "all")
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
