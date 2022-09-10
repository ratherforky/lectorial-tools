module Web.Controller.Prelude
( module Web.Types
, module Application.Helper.Controller
, module IHP.ControllerPrelude
, module Generated.Types
, (.>)
)
where

import Web.Types
import Application.Helper.Controller
import IHP.ControllerPrelude
import Generated.Types
import Web.Routes
import Control.Category (Category)

(.>) :: Category cat => cat a b -> cat b c -> cat a c
(.>) = flip (.)