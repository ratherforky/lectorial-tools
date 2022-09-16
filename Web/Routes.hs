module Web.Routes where
import IHP.RouterPrelude
import Generated.Types
import Web.Types

-- Generator Marker
instance AutoRoute StaticController
instance AutoRoute RoomsController

instance CanRoute RoomsController where
  parseRoute' = do
    string "/Rooms/"
    let roomById = do
          id <- parseId
          endOfInput
          pure $ ShowRoomAction{ maybeRoomId = Just id, maybeFriendlyId = Nothing }
    let roomByFriendlyId = do
          friendlyId <- remainingText
          pure $ ShowRoomAction{ maybeRoomId = Nothing, maybeFriendlyId = Just friendlyId }

    roomById <|> roomByFriendlyId

instance HasPath RoomsController where
  pathTo ShowRoomAction{ maybeRoomId = Just id, maybeFriendlyId = _ } = "/Rooms/" <> tshow id
  pathTo ShowRoomAction{ maybeRoomId = Nothing, maybeFriendlyId = Just friendlyId } = "/Rooms/" <> friendlyId
