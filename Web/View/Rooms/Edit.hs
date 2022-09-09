module Web.View.Rooms.Edit where
import Web.View.Prelude

data EditView = EditView { room :: Room }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit Room</h1>
        {renderForm room}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Rooms" RoomsAction
                , breadcrumbText "Edit Room"
                ]

renderForm :: Room -> Html
renderForm room = formFor room [hsx|
    {(textField #friendlyId)}
    {submitButton}

|]