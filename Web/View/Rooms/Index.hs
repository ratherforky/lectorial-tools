module Web.View.Rooms.Index where
import Web.View.Prelude

data IndexView = IndexView { rooms :: [Room]  }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>Index<a href={pathTo NewRoomAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Room</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach rooms renderRoom}</tbody>
            </table>
            
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Rooms" RoomsAction
                ]

renderRoom :: Room -> Html
renderRoom room = [hsx|
    <tr>
        <td>{room}</td>
        <td><a href={ShowRoomAction room.id}>Show</a></td>
        <td><a href={EditRoomAction room.id} class="text-muted">Edit</a></td>
        <td><a href={DeleteRoomAction room.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]