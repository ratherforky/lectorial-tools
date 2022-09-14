module Web.View.Rooms.Index where
import Web.View.Prelude

data IndexView = IndexView { randomNewRoomId :: Text  } -- rooms :: [Room]

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>Join Room</h1>

        <form id="" method="POST" action={JoinRoomAction} data-disable-javascript-submission="true">
            <input type="text" name="friendlyId" value={randomNewRoomId}/>
            <input type="submit" class="btn btn-primary" value="Join"/>
        </form>

    |] -- TODO: Investigate why this page is merging with ShowRoom if you click back and forth. Turbolinks or morphdom issue maybe?
       -- Yes, it's to do with AJAX form submission: https://ihp.digitallyinduced.com/Guide/form.html#disable-form-submission-via-javascript
       -- The forms using the same id also confused morphdom
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Rooms" RoomsAction
                ]

-- <h1>Index<a href={pathTo NewRoomAction} class="btn btn-primary ml-4">+ New</a></h1>
-- <div class="table-responsive">
--     <table class="table">
--         <thead>
--             <tr>
--                 <th>Room</th>
--                 <th></th>
--                 <th></th>
--                 <th></th>
--             </tr>
--         </thead>
--         <tbody>{forEach rooms renderRoom}</tbody>
--     </table>
    
-- </div> 

-- renderRoom :: Room -> Html
-- renderRoom room = [hsx|
--     <tr>
--         <td>{room}</td>
--         <td><a href={ShowRoomAction room.id}>Show</a></td>
--         <td><a href={EditRoomAction room.id} class="text-muted">Edit</a></td>
--         <td><a href={DeleteRoomAction room.id} class="js-delete text-muted">Delete</a></td>
--     </tr>
-- |]