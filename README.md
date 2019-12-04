# ansible-runner

Construct, run, and collect output from Ansible adhoc commands with Haskell.

Experimental, not ready for real-world usage.

Built with:
* [stack](https://docs.haskellstack.org/en/stable/README/)
* [typed-process](http://hackage.haskell.org/package/typed-process)
* [aeson](http://hackage.haskell.org/package/aeson)
* [colog-core](http://hackage.haskell.org/package/co-log-core)

## construction Ansible actions
``` haskell
-- | Manually constructing Ansible actions
>>> :t runAdhoc (AnsibleCmd { ansibleModule = (Module "shell"), ansibleArgs = Just "uname -a" })
runAdhoc (AnsibleCmd { ansibleModule = (Module "shell"), ansibleArgs = Just "uname -a" })
  :: Pattern -> Ansible (Maybe (Results m))

-- | Phantom types for module/output information (using TypeApplications)
>>> :t runAdhoc @"shell" (AnsibleCmd { ansibleModule = (Module "shell"), ansibleArgs = Just "uname -a" })
runAdhoc @"shell" (AnsibleCmd { ansibleModule = (Module "shell"), ansibleArgs = Just "uname -a" })
  :: Pattern -> Ansible (Maybe (Results "shell"))

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

## extend ansible-runner to support arbitrary modules

Different Ansible modules provide different JSON structures. `runAdhoc` provides
an Aeson `Value` for each `Host` in a `Task`.

To facilitate different Aeson instances for each module, there is a phantom type
parameter in the `Results` type:
``` haskell
>>> :info Results
type role Results phantom
newtype Results (m :: ghc-prim-0.5.3:GHC.Types.Symbol)
  = Results {resultPlays :: [Ansible.Types.Play]}
```

We can use this type parameter to define a more precise Aeson instance
for a specific Ansible module with a new typeclass and `Results` instance:
``` haskell
newtype ShellStdout = ShellStdout [(Host, Maybe T.Text)]
  deriving (Show)

class Shell a where
  shellOutput :: a -> ShellStdout

shell :: Value -> A.Parser T.Text
shell = withObject "result" (.: "stdout")

instance Shell (Results "shell") where
  shellOutput output =
    coerce $
      second (A.parseMaybe shell) <$> adhocResults output
```

## running Ansible actions
``` haskell
-- | Raw JSON output
>>> runAnsible (Config Localhost WARN) (runAdhoc @"shell" (AnsibleCmd { ansibleModule = (Module "shell"), ansibleArgs = Just "uname -a" }) "localhost")
[2019-12-04 01:48:21.500166907] (WARN) Raw command: ansible -m shell -a "uname -a" localhost

Just (Results {resultPlays = [Play {playID = "54ee7548-ebd7-970f-8d23-000000000007", playName = "Ansible Ad-Hoc", playTasks = [Task {taskID = "54ee7548-ebd7-970f-8d23-000000000009", taskName = "shell", taskResults = [("localhost",Object (fromList [("_ansible_no_log",Bool False),("stdout_lines",Array [String "Linux nixos 4.19.79 #1-NixOS SMP Fri Oct 11 16:21:44 UTC 2019 x86_64 GNU/Linux"]),("changed",Bool True),("stdout",String "Linux nixos 4.19.79 #1-NixOS SMP Fri Oct 11 16:21:44 UTC 2019 x86_64 GNU/Linux"),("delta",String "0:00:00.006223"),("start",String "2019-12-04 01:48:22.780437"),("action",String "command"),("stderr",String ""),("rc",Number 0.0),("stderr_lines",Array []),("end",String "2019-12-04 01:48:22.786660"),("cmd",String "uname -a"),("invocation",Object (fromList [("module_args",Object (fromList [("chdir",Null),("stdin",Null),("stdin_add_newline",Bool True),("creates",Null),("removes",Null),("executable",Null),("warn",Bool True),("argv",Null),("strip_empty_ends",Bool True),("_raw_params",String "uname -a"),("_uses_shell",Bool True)]))]))]))]}]}]})

-- | Structured `ShellStdout`
>>>  runAnsible (Config Localhost WARN) (runShell "uname -a" "localhost")
[2019-12-03 18:26:35.471711371] (WARN) Raw command: ansible -m shell -a "uname -a" localhost

Just (ShellStdout [("localhost",Just "Linux nixos 4.19.79 #1-NixOS SMP Fri Oct 11 16:21:44 UTC 2019 x86_64 GNU/Linux")])
```

## todo
* lens for `shell` module output
* support `--extra-vars=EXTRA_VARS` flag
* support `--limit=SUBSET` flag
* support `--become` flag
