module Web.View.Rooms.AllRoomsStudents where
import Web.View.Prelude

data AllRoomsStudentsView = AllRoomsStudentsView { entries :: [(Text, Text)]}

instance View AllRoomsStudentsView where
    html AllRoomsStudentsView { .. } = [hsx|
        {breadcrumb}
        <h1>AllRoomsStudentsView</h1>

        <form method="POST" action={DeleteAllRoomsStudentsAction}>
            <input type="hidden" name="_method" value="DELETE"/>
            <button type="submit">Delete All</button>
        </form>

        <table>
          <tr>
            <th>Room name</th>
            <th>Student</th> 
          </tr>
          {forEach entries renderEntry}
        </table>
        |]
            where
                breadcrumb = renderBreadcrumb
                                [ breadcrumbLink "AllRoomsStudents" RoomsAction
                                , breadcrumbText "AllRoomsStudentsView"
                                ]

renderEntry :: (Text, Text) -> Html
renderEntry (i, name) = [hsx|
  <tr>
    <td>{i}</td>
    <td>{name}</td>
  </tr>
|]
