module Ansible.Shell
  ( ShellStdout (..),
    shellOutput,
    Shell,
  )
where

import Ansible.Types
import Data.Aeson
import qualified Data.Aeson.Types as A
import Data.Bifunctor (second)
import Data.Coerce
import qualified Data.Text as T

newtype ShellStdout = ShellStdout [(Host, Maybe T.Text)]
  deriving (Show)

class Shell a where
  shellOutput :: a -> ShellStdout

shellStdout :: Value -> A.Parser T.Text
shellStdout = withObject "result" (.: "stdout")

instance Shell (Results "shell") where
  shellOutput output =
    coerce $
      second (A.parseMaybe shellStdout) <$> adhocResults output
