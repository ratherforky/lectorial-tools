module Web.View.Rooms.Show where
import Web.View.Prelude
import Debug.Trace (traceShow, trace)
import Data.Text (unpack)

data ShowView = ShowView
  { room :: Room
  , studentPool :: [(Text, Id Student)]
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

        <h2>tinyurl.com/lectorial</h2>
        <h1>Room ID: {room.friendlyId}</h1>
        {renderModeratorStatus clientIsCreator}

        <form id="" method="POST" action={(AddStudentToRoomAction room.id)}>
            <input type="text" name="username" placeholder="Name"/>
            <input type="submit" class="btn btn-primary" value="Join Room"/>
        </form>

        <h4>Random Student: <strong>{randomStudent}</strong> </h4>

        {renderRandomPickButton clientIsCreator room.id}

        {renderStudentSessionData room.id maybeStudent}

        <h2>Student pool</h2>

        <ul class="list-group">
          {forEach studentPool (renderStudent clientIsCreator room.id)}
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

renderStudent :: Bool -> Id Room -> (Text, Id Student) -> Html
renderStudent isCreator rId (username, sId)
  | isCreator = [hsx|
      <div class="list-group-item d-flex align-items-center justify-content-between">{username} {deleteButton}</div>
    |]
  | otherwise = [hsx|<li>{username}</li>|]
  where
    -- deleteButton = buttonWithCSS "DELETE" "btn btn-light btn-sm float-sm-right" (DeleteStudentAction rId sId) "x"
    deleteButton  = [hsx|
      <form method="DELETE" action={DeleteStudentAction rId sId} target="_blank">
        <input type="submit" class="btn btn-light btn-sm float-sm-right" value="x"/>
      </form>
    |]

renderStudentSessionData :: Id Room -> Maybe StudentRoomData -> Html
renderStudentSessionData roomId = \case
  Nothing -> [hsx||]
  Just student -> [hsx|
    <h3>Your info</h3>
    <p>Name: {student.username}</p>
    {renderPoolToggle student.inAnswerPool}
  |]
  where
    renderPoolToggle :: Bool -> Html
    renderPoolToggle inAnswerPool
      | inAnswerPool = simpleButton (LeaveAnswerPoolAction roomId) "Leave Answer Pool"
      | otherwise    = simpleButton (JoinAnswerPoolAction roomId) "Join Answer Pool"

simpleButton :: RoomsController -> Text -> Html
simpleButton = buttonWithCSS "POST" "btn btn-primary"

buttonWithCSS :: Text -> Text -> RoomsController -> Text -> Html
buttonWithCSS method css action buttonText = [hsx|
  <form method={method} action={action} >
    <input type="submit" class={css} value={buttonText}/>
  </form>
|]

jsButton action = trace (unpack $ show html) html
  where
    html = [hsx|
      <script data-act={action}>
        const foo = () => {
          var acty = document.currentScript.dataset.act;
          console.log(acty);
        }
      </script>
      <button onclick="foo();">Click me</button>
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