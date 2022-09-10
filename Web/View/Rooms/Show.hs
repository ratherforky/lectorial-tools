module Web.View.Rooms.Show where
import Web.View.Prelude

data ShowView = ShowView { room :: Room, studentNames :: [Text], randomStudent :: Text }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show Room</h1>
        <p>Room ID: {room.friendlyId}</p>

        <form id="main-form" method="POST" action={(AddStudentToRoomAction room.id)}>
            <input type="text" name="username"/>
            <input type="submit" class="btn btn-primary"/>
        </form>

        <form id="main-form" method="POST" action={(SelectRandomStudentAction room.id)}>
            <input type="submit" class="btn btn-primary"/>
        </form>

        <p>Random Student: {randomStudent} </p>

        <h2>Student pool</h2>
        <ul>
          {forEach studentNames renderUserName}
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
