module Web.Controller.Rooms where

import Web.Controller.Prelude
import Web.View.Rooms.Index
import Web.View.Rooms.Show
import qualified IHP.Log as Log

import Web.Util.Random
import Web.Util.TypedSession
import Control.Lens (_18')
import Control.Monad (void)
import qualified Data.List.NonEmpty as NE
import Control.Monad.Trans.Maybe


data SessionKeys = SessionKeys
  { createdRoomID :: !(ByteString, Proxy (Id Room))
  , studentID :: !(ByteString, Proxy (Id Student))
  }

sk :: SessionKeys
sk = SessionKeys
  { createdRoomID = ("createdRoomID", Proxy)
  , studentID = ("studentID", Proxy)
  }

data AnswerPoolCommand = JoinPool | LeavePool deriving (Show, Eq)

instance Controller RoomsController where
    action JoinAnswerPoolAction{ roomId } = answerPoolAction roomId "Joining answer pool failed" JoinPool

    action LeaveAnswerPoolAction{ roomId } = answerPoolAction roomId "Leaving answer pool failed" LeavePool

    action JoinRoomAction = do
      let friendlyIdParam = param @Text "friendlyId"
      maybeRoom <- findRoomFriendlyId friendlyIdParam

      case maybeRoom of
        Just room -> do
          setSuccessMessage "Joined existing room"
          redirectTo ShowRoomAction{ roomId = room.id }
        Nothing   -> do
          newRecord @Room
            |> buildRoom
            |> ifValid \case
                Left room -> do
                  setErrorMessage "Error creating new room"
                  randomNewRoomId <- genUniqueRandomRoomId
                  render IndexView { .. } 
                Right room -> do
                    room <- room |> createRecord
                    setSessionTyped sk.createdRoomID room.id -- If this user created the room, store that info in the session
                    setSuccessMessage "Room created"
                    redirectTo ShowRoomAction{ roomId = room.id }

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

      redirectTo ShowRoomAction{ roomId }
      where
        createSelectionRecord roomId studentId
          = newRecord @RoomsStudentsSelected
              |> set #roomId roomId
              |> set #studentId studentId
              |> createRecord
              |> void -- Don't need to use record immediately after creation

    action AddStudentToRoomAction { roomId } = do
      Log.debug ("AddStudentToRoomAction fired with roomId =" <> show roomId)

      maybeStudent <- runMaybeT $ do
        sID <- MaybeT $ getSessionTyped sk.studentID
        MaybeT $ getRoomsStudent roomId sID
        MaybeT $ query @Student
                   |> filterWhere (#id, sID)
                   |> fetchOneOrNothing

      case maybeStudent of
        -- If student already set a name in this room
        Just student -> do
          student
            |> fill @'["username"]
            -- TODO: Validate here?
            |> updateRecord
            |> void
          -- let unameMaybe = paramOrNothing "username"
          -- case unameMaybe of
          --   Nothing -> setErrorMessage "No username param"
          --   Just uname -> 
          --     undefined
                -- |> set #

          -- roomsStudent

        -- If no student ID already
        Nothing -> do
          newRecord @Student
            |> fill @'["username"]
            |> ifValid \case
                Left _ -> setErrorMessage "Failed to add student to room"
                Right studentPreCreation -> do
                  student <- studentPreCreation |> createRecord

                  newRecord @RoomsStudent
                    |> set #roomId roomId
                    |> set #studentId student.id
                    |> set #inAnswerPool True -- Those who add their name are included by default
                    |> createRecord
                    |> void

                  -- Save student ID in session so they can edit things
                  -- related to themselves, eg. their willingness to answer questions
                  setSessionTyped sk.studentID student.id

      redirectTo ShowRoomAction{ roomId }

    action RoomsAction = do
        randomNewRoomId <- genUniqueRandomRoomId
        render IndexView { .. }

    action ShowRoomAction { roomId } = autoRefresh do
      room <- fetch roomId

      willingStudents <- queryWillingStudents room.id |> fetch
      let studentPool = map (\s -> (s |> get #username, s |> get #id)) willingStudents -- Wish this could more easily be done in the query builder

      selectedStudents <- getSelectedStudents room.id
      let randomStudent = maybe "" (\student -> student.username) (head selectedStudents)

      clientIsCreator <- checkIfClientIsCreator room.id
      Log.debug $ "Client is room creator: " <> show clientIsCreator

      maybeStudent <- getMaybeStudent room.id

      render ShowView { .. }
      where
        checkIfClientIsCreator roomId
          = maybe False (\createdRoomID -> createdRoomID == roomId)
            <$> getSessionTyped sk.createdRoomID

        getMaybeStudent roomId = runMaybeT do
          studentID <- MaybeT $ getSessionTyped sk.studentID
          student <- MaybeT $ fetchOneOrNothing studentID
          roomsStudent
            <- MaybeT
                $ query @RoomsStudent
                  |> filterWhere (#roomId, roomId)
                  |> filterWhere (#studentId, studentID)
                  |> fetchOneOrNothing
          
          pure $ StudentRoomData
                  { username = student.username
                  , inAnswerPool = roomsStudent.inAnswerPool
                  }

    action DeleteStudentAction { roomId, studentId } = do
      deleteRoomStudent roomId studentId
      -- setSuccessMessage "Post deleted"
      respondHtml [hsx|
        <!-- <!DOCTYPE html>
        <html>
          <body> -->
            <a href={ShowRoomAction roomId}>Click to go back</a>
          <!-- </body>
        </html> -->
      |]
      -- redirectToPath "/" -- ShowRoomAction{ roomId }
    -- action DeleteRoomAction { roomId } = do
    --     room <- fetch roomId
    --     deleteRoomSelections roomId
    --     deleteRoomStudents roomId
    --     deleteRecord room
    --     setSuccessMessage "Room deleted"
    --     redirectTo RoomsAction

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

deleteRoomStudent :: (?modelContext::ModelContext) => Id Room -> Id Student -> IO ()
deleteRoomStudent roomId studentId
  = sqlExec "DELETE FROM rooms_students \
            \WHERE room_id = ? AND student_id = ?" (roomId, studentId)
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

getRoomsStudent roomId studentID
  = query @RoomsStudent
    |> filterWhere (#roomId, roomId)
    |> filterWhere (#studentId, studentID)
    |> fetchOneOrNothing

getSessionStudent :: (?context::ControllerContext, ?modelContext::ModelContext)
                  => IO (Maybe Student)
getSessionStudent = runMaybeT do
  studentID <- MaybeT $ getSessionTyped sk.studentID
  MaybeT $ fetchOneOrNothing studentID

answerPoolAction roomId errorMessage answerPoolCommand = do
  maybeRoomsStudent <- runMaybeT do
    studentID <- MaybeT $ getSessionTyped sk.studentID
    MaybeT $ getRoomsStudent roomId studentID

  case maybeRoomsStudent of
    Nothing -> do
      setErrorMessage errorMessage
    Just roomsStudent -> do
      roomsStudent
      |> set #inAnswerPool (actionToBool answerPoolCommand)
      |> updateRecord
      |> void
  
  redirectTo ShowRoomAction{ roomId }
  where
    actionToBool = \case
      LeavePool -> False
      JoinPool  -> True

findRoomFriendlyId friendlyId = query @Room |> findMaybeBy #friendlyId friendlyId
