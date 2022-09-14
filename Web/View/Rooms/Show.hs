module Web.View.Rooms.Show where
import Web.View.Prelude

data ShowView = ShowView { room :: Room, studentNames :: [Text], randomStudent :: Text }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}

        <h1>Room ID: {room.friendlyId}</h1>

        <form id="" method="POST" action={(AddStudentToRoomAction room.id)}>
            <input type="text" name="username" placeholder="Name"/>
            <input type="submit" class="btn btn-primary" value="Join Room"/>
        </form>

        <form id="" method="POST" action={(SelectRandomStudentAction room.id)}>
            <input type="submit" class="btn btn-primary" value="Random Pick"/>
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
