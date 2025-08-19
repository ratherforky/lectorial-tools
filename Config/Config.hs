module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig

import qualified IHP.Log as Log
import IHP.Log.Types

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")
