# ansible-runner

Construct, run, and collect output from Ansible adhoc commands with Haskell.

built with:
* [stack](https://docs.haskellstack.org/en/stable/README/)
* [typed-process](http://hackage.haskell.org/package/typed-process)
* [colog-core](http://hackage.haskell.org/package/co-log-core)

## usage
``` haskell
>>> :t runAdhoc (shell "uname -a") "localhost"
runAdhoc (shell "uname -a") "localhost" :: Ansible AdhocOutput
>>> :t adhocOutput <$> runAnsible (Config Localhost) (runAdhoc (shell "uname -a") "localhost")
adhocOutput <$> runAnsible (Config Localhost) (runAdhoc (shell "uname -a") "localhost")
  :: IO BL.ByteString
```

## todo
* parse adhoc output with Aeson
* support `--extra-vars=EXTRA_VARS` flag
* support `--limit=SUBSET` flag
* support `--become` flag
