module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)


data StaticController = WelcomeAction deriving (Eq, Show, Data)

data RoomsController
    = RoomsAction
    | NewRoomAction
    | ShowRoomAction { roomId :: !(Id Room) }
    | CreateRoomAction
    | EditRoomAction { roomId :: !(Id Room) }
    | UpdateRoomAction { roomId :: !(Id Room) }
    | DeleteRoomAction { roomId :: !(Id Room) }
    | AddStudentToRoomAction { roomId :: !(Id Room) }
    | SelectRandomStudentAction { roomId :: !(Id Room) }
    deriving (Eq, Show, Data)
