module Web.Controller.Rooms where

import Web.Controller.Prelude
import Web.View.Rooms.Index
import Web.View.Rooms.New
import Web.View.Rooms.Edit
import Web.View.Rooms.Show

instance Controller RoomsController where
    action AddStudentToRoomAction = do
        redirectTo RoomsAction

    action RoomsAction = do
        rooms <- query @Room |> fetch
        render IndexView { .. }

    action NewRoomAction = do
        let room = newRecord
        render NewView { .. }

    action ShowRoomAction { roomId } = do
        room <- fetch roomId
        let student = newRecord
        render ShowView { .. }

    action EditRoomAction { roomId } = do
        room <- fetch roomId
        render EditView { .. }

    action UpdateRoomAction { roomId } = do
        room <- fetch roomId
        room
            |> buildRoom
            |> ifValid \case
                Left room -> render EditView { .. }
                Right room -> do
                    room <- room |> updateRecord
                    setSuccessMessage "Room updated"
                    redirectTo EditRoomAction { .. }

    action CreateRoomAction = do
        let room = newRecord @Room
        room
            |> buildRoom
            |> ifValid \case
                Left room -> render NewView { .. } 
                Right room -> do
                    room <- room |> createRecord
                    setSuccessMessage "Room created"
                    redirectTo RoomsAction

    action DeleteRoomAction { roomId } = do
        room <- fetch roomId
        deleteRecord room
        setSuccessMessage "Room deleted"
        redirectTo RoomsAction

buildRoom room = room
    |> fill @'["friendlyId"]
