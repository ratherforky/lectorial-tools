module Web.Controller.Rooms where

import Web.Controller.Prelude
import Web.View.Rooms.Index
import Web.View.Rooms.New
import Web.View.Rooms.Edit
import Web.View.Rooms.Show
import qualified IHP.Log as Log

import Web.Util.Random

instance Controller RoomsController where
    action SelectRandomStudentAction { roomId } = do
      students <- getStudentsInRoom roomId
      -- TODO: Filter students who don't want to be selected
      -- TODO: Ensure fairness
      Just randomStudent <- randomChooseMaybeIO students
      roomStudentSelected
        <- newRecord @RoomsStudentsSelected
           |> set #roomId roomId
           |> set #studentId randomStudent.id
           |> createRecord
       
      redirectTo ShowRoomAction{roomId}

    action AddStudentToRoomAction { roomId } = do
      Log.debug ("AddStudentToRoomAction fired with roomId =" <> show roomId)
      student <- newRecord @Student
                 |> fill @'["username"]
                 |> createRecord

      roomsStudent <- newRecord @RoomsStudent
                      |> set #roomId roomId
                      |> set #studentId student.id
                      |> createRecord

      redirectTo (ShowRoomAction{roomId}) -- Maybe studentId should be passed in too 

    action RoomsAction = do
        rooms <- query @Room |> fetch
        render IndexView { .. }

    action NewRoomAction = do
        let room = newRecord
        render NewView { .. }

    action ShowRoomAction { roomId } = autoRefresh do
        room <- fetch roomId
        students <- getStudentsInRoom roomId
        let studentNames = map (get #username) students -- Wish this could more easily be done in the query builder
        selectedStudents <- getSelectedStudents roomId
        let randomStudent = maybe "" (\student -> student.username) (head selectedStudents)
        Log.debug (show studentNames)
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

queryStudentsInRoom roomId
  = query @Student
    |> innerJoin @RoomsStudent (#id, #studentId)
    |> innerJoinThirdTable @Room @RoomsStudent (#id, #roomId)
    |> filterWhereJoinedTable @Room (#id, roomId)

getStudentsInRoom roomId
  = queryStudentsInRoom roomId
    |> fetch

getSelectedStudents roomId
  = query @Student
    |> innerJoin @RoomsStudentsSelected (#id, #studentId)
    |> innerJoinThirdTable @Room @RoomsStudentsSelected (#id, #roomId)
    |> filterWhereJoinedTable @Room (#id, roomId)
    |> labelResults @RoomsStudentsSelected #createdAt
    -- |> orderBy #createdAt
    |> fetch
    |> fmap (sortOn (labelValue .> Down) .> map contentValue) -- Sort newest first

-- getSelectedStudentsSQL :: Query
getSelectedStudentsSQL
  = "SELECT s.username \
    \FROM students AS s \
    \INNER JOIN rooms_students_selected AS rss ON s.id = rss.student_id \
    \INNER JOIN rooms AS r ON rss.room_id = r.id \
    \WHERE r.id = ? \
    \ORDER BY rss.created_at DESC \
    \LIMIT 1;"

-- SELECT * FROM students AS s INNER JOIN rooms_students_selected AS rss ON s.id = rss.student_id INNER JOIN rooms AS r ON rss.room_id = r.id