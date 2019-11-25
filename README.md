# ansible-runner

Construct, run, and parse the output of Ansible adhoc commands from Haskell.

built with:
* [stack](https://docs.haskellstack.org/en/stable/README/)
* [typed-process](http://hackage.haskell.org/package/typed-process)
* [colog-core](http://hackage.haskell.org/package/co-log-core)

``` haskell
>>> :t runAdhoc (shell "uname -a") "localhost"
runAdhoc (shell "uname -a") "localhost" :: Ansible AdhocOutput
>>> :t adhocOutput <$> runAnsible (Config Localhost) (runAdhoc (shell "uname -a") "localhost")
adhocOutput <$> runAnsible (Config Localhost) (runAdhoc (shell "uname -a") "localhost")
  :: IO BL.ByteString
```
