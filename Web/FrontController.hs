module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.Rooms
import Web.Controller.Static

instance FrontController WebApplication where
    controllers = 
        [ startPage RoomsAction
        -- Generator Marker
        , parseRoute @RoomsController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
