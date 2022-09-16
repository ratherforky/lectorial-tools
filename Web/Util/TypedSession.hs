module Web.Util.TypedSession where

import BasicPrelude
import IHP.ControllerPrelude
import Data.Serialize (Serialize)

setSessionTyped
  :: (?context::ControllerContext, Serialize value)
  => (ByteString, Proxy value) -> value -> IO ()
setSessionTyped (key, Proxy) val = setSession key val

getSessionTyped
  :: (?context::ControllerContext, Serialize value)
  => (ByteString, Proxy value) -> IO (Maybe value)
getSessionTyped (key, Proxy) = getSession key
