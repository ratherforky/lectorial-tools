module Web.Controller.Static where
import Web.Controller.Prelude

instance Controller StaticController where
    action WelcomeAction = redirectTo RoomsAction
