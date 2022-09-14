module Web.Controller.Rooms where

import Web.Controller.Prelude
import Web.View.Rooms.Index
import Web.View.Rooms.New
import Web.View.Rooms.Edit
import Web.View.Rooms.Show
import qualified IHP.Log as Log

import Web.Util.Random
import Control.Lens (_18')
import Control.Monad (void)
import qualified Data.List.NonEmpty as NE

instance Controller RoomsController where
    action JoinRoomAction = do
      let friendlyIdParam = param @Text "friendlyId"
      maybeRoom <- query @Room |> findMaybeBy #friendlyId friendlyIdParam

      case maybeRoom of
        Just room -> do
          setSuccessMessage "Joined existing room"
          redirectTo ShowRoomAction{ roomId = room.id }
        Nothing   -> do
          let room = newRecord @Room
          room
            |> buildRoom
            |> ifValid \case
                Left room -> do
                  setErrorMessage "Error creating new room"
                  randomNewRoomId <- genUniqueRandomRoomId
                  render IndexView { .. } 
                Right room -> do
                    room <- room |> createRecord
                    setSuccessMessage "Room created"
                    redirectTo ShowRoomAction{roomId = room.id}

      -- redirectTo RoomsAction

    action SelectRandomStudentAction { roomId } = do
      students :: [Student] <- queryWillingStudents roomId |> fetch
      case students of
        [] -> do
          setErrorMessage "No students to select from"
          -- Log.debug ("No willing students to select from" :: String) -- TODO: Add more info to message with interpolation
        (x:xs) -> do
          selectedStudents <- getSelectedStudents roomId
          let remainingStudents = students \\ selectedStudents
          randomStudent
            <- case remainingStudents of
                [] -> do
                  deleteRoomSelections roomId -- Remove all selections from DB and start again
                  randomChooseIO (x NE.:| xs)
                (a:as) -> randomChooseIO (a NE.:| as)
          
          createSelectionRecord roomId randomStudent.id

      redirectTo ShowRoomAction{roomId}
      where
        createSelectionRecord roomId studentId
          = newRecord @RoomsStudentsSelected
              |> set #roomId roomId
              |> set #studentId studentId
              |> createRecord
              |> void -- Don't need to use record immediately after creation

    action AddStudentToRoomAction { roomId } = do
      Log.debug ("AddStudentToRoomAction fired with roomId =" <> show roomId)
      student <- newRecord @Student
                 |> fill @'["username"]
                 |> createRecord

      roomsStudent <- newRecord @RoomsStudent
                      |> set #roomId roomId
                      |> set #studentId student.id
                      |> set #inAnswerPool True -- Those who add their name are included by default
                      |> createRecord

      redirectTo (ShowRoomAction{roomId}) -- Maybe studentId should be passed in too 

    action RoomsAction = do
        randomNewRoomId <- genUniqueRandomRoomId
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
        deleteRoomSelections roomId
        deleteRoomStudents roomId
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

queryWillingStudents roomId
  = queryStudentsInRoom roomId
    |> filterWhereJoinedTable @RoomsStudent (#inAnswerPool, True)

getSelectedStudents roomId
  = query @Student
    |> innerJoin @RoomsStudentsSelected (#id, #studentId)
    |> innerJoinThirdTable @Room @RoomsStudentsSelected (#id, #roomId)
    |> filterWhereJoinedTable @Room (#id, roomId)
    |> labelResults @RoomsStudentsSelected #createdAt
    -- |> orderBy #createdAt
    |> fetch
    |> fmap (sortOn (labelValue .> Down) .> map contentValue) -- Sort newest first

-- resetRoomSelections roomId
--   = sqlExec "DELETE FROM rooms_students_selected WHERE room_id = ?" roomId

deleteRoomSelections :: (?modelContext::ModelContext) => Id Room -> IO ()
deleteRoomSelections roomId
  = sqlExec "DELETE FROM rooms_students_selected \
            \WHERE room_id = ?" (Only roomId)
    |> void

deleteRoomStudents :: (?modelContext::ModelContext) => Id Room -> IO ()
deleteRoomStudents roomId
  = sqlExec "DELETE FROM rooms_students \
            \WHERE room_id = ?" (Only roomId)
    |> void

-- getSelectedStudentsSQL :: Query
getSelectedStudentsSQL
  = "SELECT s.username \
    \FROM students AS s \
    \INNER JOIN rooms_students_selected AS rss ON s.id = rss.student_id \
    \INNER JOIN rooms AS r ON rss.room_id = r.id \
    \WHERE r.id = ? \
    \ORDER BY rss.created_at DESC \
    \LIMIT 1;"

genUniqueRandomRoomId = pure "RandomID" -- TODO: Make this random and ensure it doesn't already exist in DB