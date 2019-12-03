# ansible-runner

Construct, run, and collect output from Ansible adhoc commands with Haskell.

Experimental, not ready for real-world usage.

Built with:
* [stack](https://docs.haskellstack.org/en/stable/README/)
* [typed-process](http://hackage.haskell.org/package/typed-process)
* [aeson](http://hackage.haskell.org/package/aeson)
* [colog-core](http://hackage.haskell.org/package/co-log-core)

## usage
``` haskell
-- | Manually consturcting Ansible actions
>>> :t runAdhoc (AnsibleCmd { ansibleModule = (Module "shell"), ansibleArgs = Just "uname -a" })
runAdhoc (AnsibleCmd { ansibleModule = (Module "shell"), ansibleArgs = Just "uname -a" })
  :: Pattern -> Ansible (Maybe (Results m))

-- | Using utility functions to construct well-typed Ansible actions
>>> :t runShell "uname -a"
runShell "uname -a" :: Pattern -> Ansible (Maybe ShellStdout)

-- | Run Ansible commands against localhost with "WARN" logging level
>>> :t runAnsible (Config Localhost WARN)
runAnsible (Config Localhost WARN) :: Ansible a -> IO a

-- | Run "uname -a" locally and return the result
>>> :t runAnsible (Config Localhost WARN) (runShell "uname -a" "localhost")
runAnsible (Config Localhost WARN) (runShell "uname -a" "localhost")
  :: IO (Maybe ShellStdout)
```

``` haskell
>>>  runAnsible (Config Localhost WARN) (runShell "uname -a" "localhost")
[2019-12-03 18:26:35.471711371] (WARN) Raw command: ansible -m shell -a "uname -a" localhost

Just (ShellStdout [("localhost",Just "Linux nixos 4.19.79 #1-NixOS SMP Fri Oct 11 16:21:44 UTC 2019 x86_64 GNU/Linux")])
```

## todo
* lens for `shell` module output
* support `--extra-vars=EXTRA_VARS` flag
* support `--limit=SUBSET` flag
* support `--become` flag
