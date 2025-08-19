module Web.Controller.Prelude
( module Web.Types
, module Application.Helper.Controller
, module IHP.ControllerPrelude
, module Generated.Types
, (.>)
, adminAuth
)
where

import Web.Types
import Application.Helper.Controller
import IHP.ControllerPrelude
import Generated.Types
import Web.Routes
import Control.Category (Category)
import IHP.RouterPrelude (readFileUtf8)
import qualified IHP.Log as Log

(.>) :: Category cat => cat a b -> cat b c -> cat a c
(.>) = flip (.)

adminAuth :: (?context::ControllerContext) => IO ()
adminAuth = do
  -- The password secret is managed by agenix.
  -- The encrypted password in secrets/admin-password.age is
  -- decrypted at runtime and put in "/run/agenix/admin-password"
  pass <- readFileUtf8 "/run/agenix/admin-password"
  Log.debug ("Password: " <> pass)
  -- basicAuth "admin" "password" ""
  basicAuth "admin" pass ""
