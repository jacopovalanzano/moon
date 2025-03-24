{-# LANGUAGE RankNTypes #-}
module Moon.Database.SQL where

import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Data.String (fromString)
import Data.Int (Int64)

{- Define the Database record that holds the connection details -}
data SQLDatabase = SQLDatabase {
    dbConnection :: Connection,
    dbName       :: String,
    dbHost       :: String,
    dbUser       :: String,
    dbPassword   :: String
}

{- Open function to initialize a connection to the database -}
open :: SQLDatabase -> IO SQLDatabase
open db = do
    let connInfo = Database.MySQL.Simple.ConnectInfo {
            connectHost = dbHost db,
            connectPort = 3306,
            connectUser = dbUser db,
            connectPassword = dbPassword db,
            connectDatabase = dbName db,
            connectOptions = [],
            connectPath = "",
            connectSSL = Nothing
        }
    conn <- Database.MySQL.Simple.connect connInfo
    putStrLn "Connected"
    return db { dbConnection = conn }

{- Close the database connection -}
quit :: SQLDatabase -> IO ()
quit db = do
    close (dbConnection db)
    putStrLn "Connection closed"

{-  Run a query and return a list of results.
   `SQLDatabase` is the record we defined above.
   `IO [r]` is a list of results
 -}
ask :: forall r. Database.MySQL.Simple.QueryResults.QueryResults r => SQLDatabase -> String -> IO [r]
ask db sql = Database.MySQL.Simple.query_ (dbConnection db) (fromString sql)

{- Run a query and return an integer Int64 (eg. DELETE, INSERT, UPDATE) -}
exe :: SQLDatabase -> String -> IO Int64
exe db sql = Database.MySQL.Simple.execute (dbConnection db) (fromString sql) ()