{-# LANGUAGE OverloadedStrings #-}
module Main where

{- Import modules -}
import Moon.Database.SQL
import Moon.Routing.Router (route, routeS, genHtml, genJson, header, param, request)
import Moon.Session.Handler (createSessionId, getData, setData, newSession)
import Moon.Session.SessionDriver
import Network.Socket (SockAddr(..)) -- Used to get the IP from the request
import Network.Wai (remoteHost) -- Used to get the IP from the request
import Data.Text (pack)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Web.Scotty as ScottyM
import Web.Scotty (scotty, ScottyM)

{- Create a new MemorySessionDriver -}
newMemorySessionDriver :: IO Moon.Session.SessionDriver.MemorySessionDriver
newMemorySessionDriver = return $ Moon.Session.SessionDriver.MemorySessionDriver Moon.Session.SessionDriver.inMemoryStore

{- Create a new FileSessionDriver -}
newFileSessionDriver :: FilePath -> IO Moon.Session.SessionDriver.FileSessionDriver
newFileSessionDriver path = return $ Moon.Session.SessionDriver.FileSessionDriver path

main :: IO ()
main = do

    -- Initialize the Database with connection details. Adjust the connection details to match your database.
    let db = Moon.Database.SQL.SQLDatabase { dbConnection = undefined
    , dbName = "gogs"
    , dbHost = "127.0.0.1"
    , dbUser = "pc"
    , dbPassword = "supersecure"
    }

    -- Open a connection
    db' <- Moon.Database.SQL.open db

    -- Execute query ( returns a list of tuples )
    result <- Moon.Database.SQL.ask db' "SELECT id,name FROM user WHERE 1=1;" :: IO [(Int, String)]

    -- Print the result
    print result

    -- Close the connection
    Moon.Database.SQL.quit db'

    -- Create a simple HTML output
    let htmlString = Moon.Routing.Router.genHtml $ TL.concat
            ["<h1>Chivalry</h1>"
            ,"<p>Test Page</p>"
            ]

    -- Define static and dynamic routes
    let testRoutes :: Web.Scotty.ScottyM ()
        testRoutes = do
            -- Static routes
            Moon.Routing.Router.routeS "get" "/test" htmlString
            Moon.Routing.Router.routeS "get" "/test/static" $ Moon.Routing.Router.genHtml "<h1>Static Route Test Page</h1>"

            -- Dynamic route with :id
            Moon.Routing.Router.route "get" "/test/:id" $ do
                id <- Moon.Routing.Router.param "id"  -- Extract :id from the URL
                Moon.Routing.Router.genHtml $ TL.concat
                    ["<h1>Infantry</h1>"
                    ,"<p>ID: ", id, "</p>"
                    ]

            -- Static route (POST)
            Moon.Routing.Router.routeS "post" "/test" $ do
                -- Return a JSON response
                Moon.Routing.Router.genJson $ TL.toStrict $ TL.pack "{\"message\": \"Hello World\"}"

            -- Dynamic route
            Moon.Routing.Router.route "get" "/" $ do
                -- Get IP
                request_ <- Moon.Routing.Router.request
                let remoteIP = case Network.Wai.remoteHost request_ of
                        Network.Socket.SockAddrInet _ h -> T.pack $ show h -- h is the host
                        _ -> T.pack "unknown"  -- Use T.pack for Text

                -- Get User-Agent from headers
                userAgent_ <- Moon.Routing.Router.header (TL.pack "User-Agent")  -- TL.Text for header
                let userAgent = maybe (T.pack "unknown") TL.toStrict userAgent_  -- Convert to strict Text

                -- Create a session ID
                let sessionId = Moon.Session.Handler.createSessionId remoteIP userAgent

                -- Create a memory-based session
                memDriver <- Control.Monad.IO.Class.liftIO $ newMemorySessionDriver
                let memSession = Moon.Session.Handler.newSession sessionId memDriver

                -- Create a file-based session
                fileDriver <- Control.Monad.IO.Class.liftIO $ newFileSessionDriver "/tmp/haskell/sessions"
                let fileSession = Moon.Session.Handler.newSession sessionId fileDriver

                -- Get session data
                memData <- Control.Monad.IO.Class.liftIO $ Moon.Session.Handler.getData memSession
                fileData <- Control.Monad.IO.Class.liftIO $ Moon.Session.Handler.getData fileSession

                -- Update visit count RAM
                let (newMemData, memVisits) = case memData of
                        Just data_ ->
                            let visits = read (Map.findWithDefault "0" "visits" data_) :: Int
                                newMemData = Map.insert "visits" (show $ visits + 1) data_
                            in (newMemData, visits + 1) -- Populate newMemData
                        Nothing ->
                            let newMemData = Map.singleton "visits" "1"
                            in (newMemData, 1) -- Populate memVisits

                -- Update visit count File
                let (newFileData, fileVisits) = case fileData of
                        Just data_ ->
                            let visits = read (Map.findWithDefault "0" "visits" data_) :: Int
                                newFileData = Map.insert "visits" (show $ visits + 1) data_
                            in (newFileData, visits + 1)
                        Nothing ->
                            let newFileData = Map.singleton "visits" "1"
                            in (newFileData, 1)

                -- Update session data
                Control.Monad.IO.Class.liftIO $ Moon.Session.Handler.setData memSession newMemData
                Control.Monad.IO.Class.liftIO $ Moon.Session.Handler.setData fileSession newFileData

                -- Generate HTML
                Moon.Routing.Router.genHtml $ TL.concat
                            [ TL.pack "<!doctype html><html><body>"
                            , TL.pack "<h1>Hello, World!</h1>"
                            , TL.pack "<p>Session ID: ", TL.fromStrict sessionId, TL.pack "</p>"
                            , TL.pack "<p>Visits (file): ", TL.pack $ show $ fileVisits, TL.pack "</p>"
                            , TL.pack "<p>Visits (RAM): ", TL.pack $ show $ memVisits, TL.pack "</p>"
                            , TL.pack "<p>IP: ", TL.fromStrict remoteIP, TL.pack "</p>"
                            , TL.pack "<p>User-Agent: ", TL.fromStrict userAgent, TL.pack "</p>"
                            , TL.pack "</body></html>"
                            ]

    Web.Scotty.scotty 3000 $ testRoutes