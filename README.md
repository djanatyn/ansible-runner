# ansible-runner

Construct, run, and collect output from Ansible adhoc commands with Haskell.

Experimental, not ready for real-world usage.

Built with:
* [stack](https://docs.haskellstack.org/en/stable/README/)
* [typed-process](http://hackage.haskell.org/package/typed-process)
* [aeson](http://hackage.haskell.org/package/aeson)
* [colog-core](http://hackage.haskell.org/package/co-log-core)

## constructing Ansible actions
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

## Extending ansible-runner to support arbitrary modules
### Types for Specific Modules
While the `ansible` command returns consistently structured JSON for tasks and
plays, different Ansible *modules* provide different JSON structures.

To allow running any module, `runAdhoc` returns an Aeson `Value` for each `Host`
in a `Task` via the `Results` type.

Parsing an Aeson `Value` from every `Results` value is very inconvenient! A more
structured type for fetching the "shell" module's stdout (for example) is preferred:
``` haskell
newtype ShellStdout = ShellStdout [(Host, Maybe T.Text)]
  deriving (Show)
```

However, it should only be possible to construct a `ShellStdout` value from
"shell" module invocations.

### Module Phantom Types
To accomplish this, a type-level `Symbol` phantom type parameter represents the
name of the Ansible module invoked in the `AnsibleCmd` and `Results` types:
``` haskell
>>> :info AnsibleCmd
type role AnsibleCmd phantom
data AnsibleCmd (m :: ghc-prim-0.5.3:GHC.Types.Symbol)
  = AnsibleCmd {ansibleModule :: Module m, ansibleArgs :: Maybe Text}
...
>>> :info Results
type role Results phantom
newtype Results (m :: ghc-prim-0.5.3:GHC.Types.Symbol)
  = Results {resultPlays :: [Ansible.Types.Play]}
```

When using `runAdhoc`, the parameter in `AnsibleCmd` and `Results` must match:
``` haskell
>>> :t runAdhoc
runAdhoc :: AnsibleCmd m -> Pattern -> Ansible (Maybe (Results m))
>>> :t runAdhoc @"shell"
runAdhoc @"shell"
  :: AnsibleCmd "shell"
     -> Pattern -> Ansible (Maybe (Results "shell"))
```

### Parsing (Only) "shell" Module Invocations
The `Results` type parameter can be used to parse the "stdout" JSON field for
*shell module invocations only*, with the help of the GHC `FlexibleInstances`
extension: 
``` haskell
class Shell a where
  shellOutput :: a -> ShellStdout

shellOutput :: Value -> A.Parser T.Text
shellOutput = withObject "result" (.: "stdout")

instance Shell (Results "shell") where
  shellOutput output =
    coerce $
      second (A.parseMaybe shellOutput) <$> adhocResults output
```

## todo
* support `--extra-vars=EXTRA_VARS` flag
* support `--limit=SUBSET` flag
* support `--become` flag
