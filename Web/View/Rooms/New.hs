module Web.View.Rooms.New where
import Web.View.Prelude

data NewView = NewView { room :: Room }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New Room</h1>
        {renderForm room}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Rooms" RoomsAction
                , breadcrumbText "New Room"
                ]

renderForm :: Room -> Html
renderForm room = formFor room [hsx|
    {(textField #friendlyId)}
    {submitButton}

|]