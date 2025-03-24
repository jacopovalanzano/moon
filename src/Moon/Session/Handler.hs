{-# LANGUAGE ExistentialQuantification #-}
module Moon.Session.Handler (createSessionId, getData, setData, newSession, Session) where

import Moon.Session.SessionDriver
import Crypto.Hash (SHA256(..), hashWith)
import qualified Data.ByteString.Char8 as BC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

{- Define the SessionId and SessionData types -}
type SessionId = Text
type SessionData = Map String String

{- Existential wrapper: stores the driver implementation -}
data SessionDriverObj = forall a . SessionDriver a => SessionDriverObj a

{- Define the Session type -}
data Session = Session {
    sessionId :: SessionId,
    data_ :: SessionData,
    driver :: SessionDriverObj
}

{- Get session data -}
getData :: Session -> IO (Maybe SessionData)
getData session = case driver session of
    SessionDriverObj d -> Moon.Session.SessionDriver.getSessionData d (sessionId session)

{- Update session data -}
setData :: Session -> SessionData -> IO ()
setData session newData = case driver session of
    SessionDriverObj d -> Moon.Session.SessionDriver.updateSessionData d (sessionId session) newData

{-- Create a new session -}
newSession :: SessionDriver a => Text -> a -> Session
newSession sid drv = Session {
    sessionId = sid,
    data_ = Map.empty,
    driver = SessionDriverObj drv
}

{- Generate a session ID -}
createSessionId :: Text -> Text -> SessionId
createSessionId remoteIP userAgent =
    let input = TE.encodeUtf8 $ remoteIP <> userAgent
        hashed = Crypto.Hash.hashWith Crypto.Hash.SHA256 input
    in TE.decodeUtf8 $ BC.pack $ show hashed