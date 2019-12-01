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
>>> :t runAdhoc (shell "whoami") "localhost"
runAdhoc (shell "whoami") "localhost"
  :: Ansible (Maybe (AdhocOutput "shell"))
>>> :t runAnsible (Config Localhost WARN) (runAdhoc (shell "whoami") "localhost")
runAnsible (Config Localhost WARN) (runAdhoc (shell "whoami") "localhost")
  :: IO (Maybe (AdhocOutput "shell"))
```

## todo
* lens for `shell` module output
* support `--extra-vars=EXTRA_VARS` flag
* support `--limit=SUBSET` flag
* support `--become` flag
