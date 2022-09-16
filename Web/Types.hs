module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)


data StaticController = WelcomeAction deriving (Eq, Show, Data)

data RoomsController
    = RoomsAction
    | ShowRoomAction { roomId :: !(Id Room) }
    | AddStudentToRoomAction { roomId :: !(Id Room) }
    | SelectRandomStudentAction { roomId :: !(Id Room) }
    | JoinRoomAction
    | LeaveAnswerPoolAction { roomId :: !(Id Room) }
    | JoinAnswerPoolAction { roomId :: !(Id Room) }
    -- | DeleteRoomAction { roomId :: !(Id Room) }
    deriving (Eq, Show, Data)
