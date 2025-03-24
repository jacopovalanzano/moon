# Session Module


The `moon` session module was built using [STM](https://hackage.haskell.org/package/stm) and
[FilePath](https://hackage.haskell.org/package/filepath) packages. It offers two types of storage:
_memory-based_  and _file-based_.

The file session data is stored in a file and will persist across server restarts. The memory session
data is stored in memory (RAM) and will be lost if the web server is stopped.


## Example usage


```haskell
{- Create a new MemorySessionDriver -}
newMemorySessionDriver :: IO Moon.Session.SessionDriver.MemorySessionDriver
newMemorySessionDriver = return $ Moon.Session.SessionDriver.MemorySessionDriver Moon.Session.SessionDriver.inMemoryStore

{- Create a new FileSessionDriver -}
newFileSessionDriver :: FilePath -> IO Moon.Session.SessionDriver.FileSessionDriver
newFileSessionDriver path = return $ Moon.Session.SessionDriver.FileSessionDriver path

-- moon/app/Main.hs
main :: IO ()
main = do

    ...

    -- Define IP and User-Agent
    let ip = "127.0.0.1"
    let ua = "Firefox 1.0"

    -- Create a session ID
    let sessionId = Moon.Session.Handler.createSessionId remoteIP userAgent

    -- Create a memory-based session
    memDriver <- liftIO $ newMemorySessionDriver
    let memSession = Moon.Session.Handler.newSession sessionId memDriver

    -- Create a file-based session
    fileDriver <- liftIO $ newFileSessionDriver "/tmp/sessions"
    let fileSession = Moon.Session.Handler.newSession sessionId fileDriver

    -- Get session data
    memData <- liftIO $ Moon.Session.Handler.getData memSession
    fileData <- liftIO $ Moon.Session.Handler.getData fileSession

    -- Print data
    liftIO $ print memData
    liftIO $ print fileData

    -- Create some data
    let someData = Map.singleton "Hello, " "World!"

    -- Populate session data
    liftIO $ Moon.Session.Handler.setData memSession someData
    liftIO $ Moon.Session.Handler.setData fileSession someData

    -- Get session data
    memData <- liftIO $ Moon.Session.Handler.getData memSession
    fileData <- liftIO $ Moon.Session.Handler.getData fileSession

    -- Print data
    liftIO $ print memData
    liftIO $ print fileData
```

### Extending the Session driver (SessionDriver)

You can extend the session driver with your own driver, for example, a database-based driver.

A mock example:

```haskell
-- Hypothetical SQL Session Driver
data SQLSessionDriver = SQLSessionDriver {
    dbConnection :: Connection
}

-- SessionDriver typeclass
instance SessionDriver SQLSessionDriver where
    getSessionData driver sid = do
        ...

    updateSessionData driver sid newData = do
        ...

-- Function to create a new SQLSessionDriver
newSQLSessionDriver :: IO SQLSessionDriver
newSQLSessionDriver = do
    ...
```

A real-world example of a custom session driver is provided below:

```haskell
-- Custom Session Driver (User-defined)
data MyCustomSessionDriver = MyCustomSessionDriver {
    customStore :: TVar (Map.Map Text (Map.Map String String))
}

instance SessionDriver MyCustomSessionDriver where
    getSessionData driver sid = atomically $ do
        store <- readTVar (customStore driver)
        return $ Map.lookup sid store

    updateSessionData driver sid newData = atomically $ do
        store <- readTVar (customStore driver)
        writeTVar (customStore driver) $ Map.insert sid newData store

-- Function to create a new MyCustomSessionDriver
newMyCustomSessionDriver :: IO MyCustomSessionDriver
newMyCustomSessionDriver = do
    store <- newTVarIO Map.empty
    return $ MyCustomSessionDriver store
```