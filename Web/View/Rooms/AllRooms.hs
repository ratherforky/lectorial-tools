module Web.View.Rooms.AllRooms where
import Web.View.Prelude

data AllRoomsView = AllRoomsView {}

instance View AllRoomsView where
    html AllRoomsView { .. } = [hsx|
        {breadcrumb}
        <h1>AllRoomsView</h1>
        |]
            where
                breadcrumb = renderBreadcrumb
                                [ breadcrumbLink "AllRooms" RoomsAction
                                , breadcrumbText "AllRoomsView"
                                ]