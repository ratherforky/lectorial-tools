module Web.View.Rooms.Show where
import Web.View.Prelude

data ShowView = ShowView { room :: Room, student :: Student }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show Room</h1>
        <p>Room ID: {room.friendlyId}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Rooms" RoomsAction
                            , breadcrumbText "Show Room"
                            ]

renderStudentForm :: Room -> Html
renderStudentForm room = formFor room [hsx|
    {(textField #friendlyId)}
    {submitButton}
|]