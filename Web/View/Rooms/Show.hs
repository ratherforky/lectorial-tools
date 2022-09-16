module Web.View.Rooms.Show where
import Web.View.Prelude

data ShowView = ShowView
  { room :: Room
  , studentPool :: [Text]
  , randomStudent :: Text
  , clientIsCreator :: Bool
  , maybeStudent :: Maybe StudentRoomData
  }

data StudentRoomData = StudentRoomData
  { username :: Text
  , inAnswerPool :: Bool
  }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}

        <h1>Room ID: {room.friendlyId}</h1>
        {renderModeratorStatus clientIsCreator}

        <form id="" method="POST" action={(AddStudentToRoomAction room.id)}>
            <input type="text" name="username" placeholder="Name"/>
            <input type="submit" class="btn btn-primary" value="Join Room"/>
        </form>

        {renderRandomPickButton clientIsCreator room.id}

        <p>Random Student: {randomStudent} </p>

        {renderStudentSessionData room.id maybeStudent}

        <h2>Student pool</h2>
        <ul>
          {forEach studentPool renderUserName}
        </ul>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Rooms" RoomsAction
                            , breadcrumbText "Show Room"
                            ]

-- renderStudentForm :: Room -> Student -> Html
-- renderStudentForm room student = formFor student [hsx|
--     {(textField #friendlyId)}
--     {submitButton}
-- |]

renderUserName :: Text -> Html
renderUserName username = [hsx|<li>{username}</li>|]

renderStudentSessionData :: Id Room -> Maybe StudentRoomData -> Html
renderStudentSessionData roomId = \case
  Nothing -> [hsx||]
  Just student -> [hsx|
    <p>Your name: {student.username}</p>
    {renderPoolToggle student.inAnswerPool}
  |]
  where
    renderPoolToggle :: Bool -> Html
    renderPoolToggle inAnswerPool
      | inAnswerPool = simpleButton (LeaveAnswerPoolAction roomId) "Leave Answer Pool"
      | otherwise    = simpleButton (JoinAnswerPoolAction roomId) "Join Answer Pool"

simpleButton :: RoomsController -> Text -> Html
simpleButton actionPOST buttonText = [hsx|
  <form method="POST" action={actionPOST}>
    <input type="submit" class="btn btn-primary" value={buttonText}/>
  </form>
|]

renderModeratorStatus :: Bool -> Html
renderModeratorStatus isCreator
  | isCreator = [hsx|
    <p>You are the moderator</p>
  |]
  | otherwise = [hsx||]

renderRandomPickButton isCreator roomId
  | isCreator = simpleButton (SelectRandomStudentAction roomId) "Random Pick"
  | otherwise = ""