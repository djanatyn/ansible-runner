# ansible-runner

Construct, run, and collect output from Ansible adhoc commands with Haskell.

built with:
* [stack](https://docs.haskellstack.org/en/stable/README/)
* [typed-process](http://hackage.haskell.org/package/typed-process)
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
* support `--extra-vars=EXTRA_VARS` flag
* support `--limit=SUBSET` flag
* support `--become` flag
