-- |
-- Module      : Main
-- Description : Run Ansible adhoc commands.
--
-- This module exports functions for constructing Ansible adhoc actions, and
-- running those actions.
--
-- >>> runAnsible (Config (Inventory "~/inv/ansible-runner.yml")) (runAdhoc ping "all")
-- [2019-11-14 20:22:59.585952] $ ansible -i ~/inv/ansible-runner.yml -m ping all
-- (ExitFailure 4,"lotus.flowercluster.io | UNREACHABLE! => {\n    \"changed\": false, \n    \"msg\": \"[Errno 8] nodename nor servname provided, or not known\", \n    \"unreachable\": true\n}\n","")
module Main where

import Ansible.Types (AnsibleCmd (..), Module (..))

-- | Run ping module.
ping :: AnsibleCmd
ping =
  AdhocCmd
    { ansibleModule = Module "ping",
      ansibleArgs = Nothing
    }

main :: IO ()
main = undefined
